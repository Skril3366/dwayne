package dwayne.parser

import cats.data.OptionT
import cats.syntax.all.*
import cats.Functor
import cats.Id
import cats.Monad
import scala.util.chaining.*

case class Text(lines: List[String])

object Text{
  def apply(s: String): Text = Text(s.split("\n").toList)
  def empty: Text = Text(List.empty)
}

case class PInput(
    text: Text,
    location: Location // should point and the beginning of the current str in the input
)

case class PResult[T](
    result: T,
    leftOver: PInput
)

object PResult {
  given Functor[PResult] with {
    override def map[A, B](fa: PResult[A])(f: A => B): PResult[B] = fa.copy(result = f(fa.result))
  }
}

trait Parser[F[_]: Monad, T] {
  def parse(in: PInput): F[PResult[T]]

  def parseFull(s: String) = parse(PInput(Text(s), Location(0, 0))).map(_.result)

  def repeatedUntil(done: F[PResult[T]] => Boolean): Parser[F, List[T]] =
    new Parser[F, List[T]] {
      def parse(in: PInput): F[PResult[List[T]]] =
        Monad[F].tailRecM((List.empty[T], in)) { case (acc, in) =>
          val res = Parser.this.parse(in)
          if (done(res)) summon[Monad[F]].pure(Right(PResult(acc, in)))
          else
            res.map { r =>
              Left((acc :+ r.result, r.leftOver)) // TODO: maybe this is not optimal because list is singly linked
            }
        }
    }

  def morph[G[_]: Monad](f: F[PResult[T]] => G[PResult[T]]): Parser[G, T] = {
    def oldParse = parse
    new Parser[G, T] {
      def parse(in: PInput): G[PResult[T]] = f(oldParse(in))
    }
  }

  def withLocation: Parser[F, (T, Location)] =
    new Parser[F, (T, Location)] {
      def parse(in: PInput): F[PResult[(T, Location)]] =
        Parser.this.parse(in).map { r =>
          r.copy(result = (r.result, in.location))
        }
    }
}

extension[F[_]: Monad](p: Parser[F, Unit]) {
  def >>[T](p2: Parser[F, T]): Parser[F, T] = for {
    _ <- p
    t <- p2
  } yield t
}

extension[T](p: Parser[Id, T]) {
  def pure[F[_]: Monad] = p.morph(_.pure)
}

object Parser {
  def parallel[F[_]: Monad, T](success: F[PResult[T]] => Boolean, parsers: Seq[Parser[F, T]]) =
    new Parser[[T] =>> OptionT[F, T], T] {
      def parse(in: PInput) = {
        def convert[A](x: Option[F[A]]): OptionT[F, A] = OptionT(x.traverse(identity))
        parsers
          .map(_.parse(in))
          .filter(success)
          .headOption
          .pipe(convert)
      }
    }

  given monad[F[_]: Monad]: Monad[[T] =>> Parser[F, T]] with {
    override def pure[A](x: A): Parser[F, A] = new Parser[F, A] {
      def parse(s: PInput): F[PResult[A]] = PResult(x, s).pure
    }

    override def flatMap[A, B](fa: Parser[F, A])(f: A => Parser[F, B]): Parser[F, B] = new Parser[F, B] {
      def parse(s: PInput): F[PResult[B]] =
        fa.parse(s).flatMap(a => f(a.result).parse(a.leftOver))
    }

    override def tailRecM[A, B](a: A)(f: A => Parser[F, Either[A, B]]): Parser[F, B] = new Parser[F, B] {
      def parse(s: PInput): F[PResult[B]] =
        f(a).parse(s).flatMap { r =>
          r.result match {
            case Left(a)  => tailRecM(a)(f).parse(r.leftOver)
            case Right(b) => r.map(_ => b).pure
          }
        }
    }
  }
}

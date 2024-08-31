package dwayne.org

import cats.syntax.all.*
import dwayne.codec.ValidatedCodec
import dwayne.org.Task
import dwayne.parser.*
import zio.{Task => _, _}
import zio.test._

object OrgCodecSpec extends ZIOSpecDefault {

  val titleLineParts: List[(Task => Boolean, String)] = List(
    (t => t.state.map(_ == "TODO").getOrElse(false), "TODO"),
    (t => t.priority === 0.some, "[#A]"),
    (t => t.title === "Some title", "Some title"),
    (t => t.tags === List("some", "tags2"), ":some:tags2:")
  )

  def generateSubLists[T](l: List[T]): List[List[T]] =
    l match {
      case Nil => List(List())
      case x :: xs =>
        val subLists = generateSubLists(xs)
        subLists ++ subLists.map(x :: _)
    }

  def makeTest(
      l: List[(Task => Boolean, String)]
  ): Spec[Any, Nothing] = {
    val titleLine = f"* ${l.map(_._2).mkString(" ")}"
    test(s"Should parse correctly '$titleLine'") {
      val parcedTask: ValidatedCodec[Task] = titleLine.decode
      val task                             = parcedTask.toOption.get
      l.foldLeft(assertTrue(true)) { case (acc, (assertion, _)) =>
        acc && assertTrue(assertion(task))
      }
    }
  }

  def spec =
    suite("OrgCodecSpec")(generateSubLists(titleLineParts).map(makeTest))
}

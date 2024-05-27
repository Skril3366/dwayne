package dwayne.data

import cats.*
import cats.implicits.*

enum Tree[A] {
  case Empty()
  case Leaf(value: A)
  case Node(value: A, children: List[Leaf[A] | Node[A]])

  def toList: List[TreeNode[A]] = this match {
    case Empty() => Nil
    case Leaf(value) => List(TreeNode(1, value))
    case Node(value, children) =>
      TreeNode(1, value)
        :: children
          .flatMap(_.toList)
          .map(n => n.copy(level = n.level + 1))
  }
}

case class TreeNode[A](
    level: Int,
    value: A
)

// TODO:
// object Tree {
//   given Traverse[Tree] with {
//     def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = fa match {
//       case Empty() => b
//       case Leaf(value) => f(b, value)
//       case Node(value, children) => {
//         children.foldLeft(f(b, value))((t, a) => foldLeft[A, B](a, t)(f))
//       }
//     }
//
//     def foldRight[A, B](fa: Tree[A], lb: Eval[B])(
//         f: (A, Eval[B]) => Eval[B]
//     ): Eval[B] = fa match {
//       case Empty() => lb
//       case Leaf(value) => f(value, lb)
//       case Node(value, children) =>
//         children.foldRight(f(value, lb))((t, a) => foldRight[A, B](t, a)(f))
//     }
//
//     def traverse[G[_], A, B](
//         fa: Tree[A]
//     )(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = fa match {
//       case Empty() => G.pure(Empty())
//       case Leaf(value) => G.map(f(value))(Leaf(_))
//       case Node(value, children) =>
//         (f(value), children.traverse(traverse(_)(f)(G))).mapN(Node(_, _))
//     }
//   }
// }

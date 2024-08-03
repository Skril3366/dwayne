package dwayne.data

import cats.*
import cats.implicits.*

// NOTE:
// Level starts at 0 and increments as a tree gets deeper for e.g.:
// 0      .
// 1     / \
// 2    /\ /\

sealed trait NodeAppendingError[+A](
    level: Int,
    value: A,
    msg: String,
    source: String
) {
  override def toString() = s"$source: $msg: $level. $value"
}

case class NodeAppendingErrorToTreeNode[+A](level: Int, value: A, msg: String)
    extends NodeAppendingError[A](level, value: A, msg, "TreeNode")

case class NodeAppendingErrorToForest[+A](level: Int, value: A, msg: String)
    extends NodeAppendingError[A](level, value: A, msg, "Forest")

case class Forest[+A](
    trees: List[TreeNode[A]]
) {
  def toList: List[TreeNodeWithLevel[A]] = trees.flatMap(_.toList)
  def appendAtLevel[B >: A](
      level: Int,
      value: B
  ): Either[NodeAppendingError[B], Forest[B]] =
    trees match {
      case _ if level < Forest.rootLevel =>
        NodeAppendingErrorToForest(level,value, "Level is less than rootLevel").asLeft
      case l if level == Forest.rootLevel =>
        Forest(l :+ (TreeNode(value))).asRight
      case Nil =>
        NodeAppendingErrorToForest(
          level,
          value,
          s"No nodes on the level above current"
        ).asLeft
      case l @ (_ :: _) =>
        l.last.appendAtLevel(level, value).map(n => Forest(l.init :+ n))

    }
}

case class TreeNode[+A](value: A, children: List[TreeNode[A]] = List.empty) {
  def toList: List[TreeNodeWithLevel[A]] = this match {
    case TreeNode(value, children) =>
      TreeNodeWithLevel(Forest.rootLevel, value)
        :: children
          .flatMap(_.toList)
          .map(n => n.copy(level = n.level + 1))
  }

  def appendAtLevel[B >: A](
      level: Int,
      value: B
  ): Either[NodeAppendingError[B], TreeNode[B]] =
    this match {
      case _ if level <= Forest.rootLevel =>
        NodeAppendingErrorToTreeNode(
          level,
          value,
          "Level is less than rootLevel"
        ).asLeft
      case root if level == Forest.rootLevel + 1 =>
        root.copy(children = root.children :+ TreeNode(value)).asRight
      case TreeNode(_, Nil) =>
        NodeAppendingErrorToTreeNode(
          level,
          value,
          "No nodes on the level above current"
        ).asLeft

      case TreeNode(v, l) =>
        l.last
          .appendAtLevel(level - 1, value)
          .map(c => TreeNode(v, l.init :+ c))
    }
}

case class TreeNodeWithLevel[+A](
    level: Int,
    value: A
) {}

object Forest {
  val rootLevel = 0
  def empty[A] = Forest[A](List.empty)

  def fromNodes[A](
      l: List[TreeNodeWithLevel[A]]
  ): Either[NodeAppendingError[A], Forest[A]] =
    l.foldLeft(empty[A].asRight) { (a, b) =>
      a.flatMap(
        _.appendAtLevel(b.level, b.value)
      )
    }
}

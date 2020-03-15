package com.fdilke.bewl2.compacta

object Cantorians {
  implicit class Cantorian(
    val stream: LazyList[Boolean]
  ) extends AnyVal {
    def apply[T](tree: GroundedTree[T]): T =
     tree match {
       case LeafNode(leaf) => leaf
       case BranchNode(left, right) =>
         apply(
           if (stream.head)
             left
           else
             right
         )
     }
  }
}

sealed trait GroundedTree[T] {
//  def
// refactor...
}

case class LeafNode[T](
  leaf: T
) extends GroundedTree[T]

case class BranchNode[T](
  left: GroundedTree[T],
  right: GroundedTree[T]
) extends GroundedTree[T]

object GroundedTree {
  def apply[T](leaf: T): GroundedTree[T] =
    LeafNode(leaf)
  def apply[T](
    left: =>GroundedTree[T],
    right: => GroundedTree[T]
  ): GroundedTree[T] =
    BranchNode(left, right)
}
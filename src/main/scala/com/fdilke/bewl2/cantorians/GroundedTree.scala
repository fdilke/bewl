package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Catcher.applyCatcher
import com.fdilke.bewl2.topology.Hausdorff

sealed trait GroundedTree[T] //
  extends CatcherFType[GroundedTree[T], Boolean, T] //
  with Function[Cantorian, T] {
  final def apply(
    cantorian: Cantorian
  ): T =
    applyCatcher(this)(cantorian)
}

case class LeafNode[T](
  leaf: T
) extends GroundedTree[T] {
  override def either: Either[T, Boolean => GroundedTree[T]] =
    Left(leaf)
}

case class BranchNode[T](
  left: GroundedTree[T],
  right: GroundedTree[T]
) extends GroundedTree[T] {

  override def either: Either[T, Boolean => GroundedTree[T]] =
    Right(switch)

  private def switch(b: Boolean): GroundedTree[T] =
    if (b)
      right
    else
      left
}

object GroundedTree {
  def apply[T](
    leaf: T
  ): GroundedTree[T] =
    LeafNode(leaf)

  def apply[T](
    left: GroundedTree[T],
    right: GroundedTree[T]
  ): GroundedTree[T] =
    BranchNode(left, right)

  implicit def catcherTude[T]: Catcher[GroundedTree[T], Boolean, T] =
    CatcherFType.standardCatcher[GroundedTree[T], Boolean, T] {
      case Left(t) => GroundedTree(t)
      case Right(bool2tree) =>
        GroundedTree(
          bool2tree(false),
          bool2tree(true)
        )
    }

  implicit def hausdorff[
    H: Hausdorff
  ]: Hausdorff[GroundedTree[H]] =
    Catcher.hausdorff[GroundedTree[H], Boolean, H]
}

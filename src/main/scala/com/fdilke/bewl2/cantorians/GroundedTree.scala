package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Catcher.applyCatcher
import com.fdilke.bewl2.topology.Hausdorff
import com.fdilke.bewl2.topology.Hausdorff.{equalH, intKey}
import GroundedTree._

sealed abstract class GroundedTree[H: Hausdorff] //
  extends CatcherFType[GroundedTree[H], Boolean, H] //
  with Function[Cantorian, H] { tree =>
  final def apply(
    cantorian: Cantorian
  ): H =
    applyCatcher(tree)(cantorian)

  override def hashCode(): Int =
    intKey(tree)

  override def equals(
    other: Any
  ): Boolean =
    equalH(
      tree,
      other.asInstanceOf[GroundedTree[H]]
    )
}

class LeafNode[H: Hausdorff](
  leaf: H
) extends GroundedTree[H] {
  override def either: Either[H, Boolean => GroundedTree[H]] =
    Left(leaf)

  override def toString: String =
    leaf.toString
}

class BranchNode[H: Hausdorff](
  left: GroundedTree[H],
  right: GroundedTree[H]
) extends GroundedTree[H] {

  override def either: Either[H, Boolean => GroundedTree[H]] =
    Right(switch)

  private def switch(b: Boolean): GroundedTree[H] =
    if (b)
      right
    else
      left

  override def toString: String =
    "<" + left.toString + ", " + right.toString + ">"
}

object GroundedTree {
  def apply[H: Hausdorff](
    leaf: H
  ): GroundedTree[H] =
    new LeafNode(leaf)

  def apply[H: Hausdorff](
    left: GroundedTree[H],
    right: GroundedTree[H]
  ): GroundedTree[H] =
    new BranchNode(left, right)

  implicit def catcherTude[H: Hausdorff]: Catcher[GroundedTree[H], Boolean, H] =
    CatcherFType.standardCatcher[GroundedTree[H], Boolean, H] {
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

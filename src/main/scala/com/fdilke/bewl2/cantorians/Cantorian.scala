package com.fdilke.bewl2.cantorians

import java.util.function.Supplier

import scala.language.postfixOps

class GroundedCatcher[T, U](
  val either: Either[U, T => GroundedCatcher[T, U]]
) extends Catcher[GroundedCatcher[T, U], T, U]

sealed trait GroundedTree[T] extends GroundedCatcher[Boolean, T] with Function[Cantorian, T]

case class LeafNode[T](
  leaf: T
) extends GroundedCatcher[Boolean, T](
      Left(leaf)
    )
    with GroundedTree[T] {
  def apply(cantorian: Cantorian): T =
    leaf
}

case class BranchNode[T](
  left: GroundedTree[T],
  right: GroundedTree[T]
) extends GroundedCatcher[Boolean, T](
      Right(boolean => if (boolean) left else right)
    )
    with GroundedTree[T] {
  def apply(cantorian: Cantorian): T =
    this.apply[Cantorian](cantorian)
}

object GroundedTree {
  def apply[T](leaf: T): GroundedTree[T] =
    LeafNode(leaf)

  def apply[T](
    left: GroundedTree[T],
    right: GroundedTree[T]
  ): GroundedTree[T] =
    BranchNode(left, right)
}

trait Cantorian extends Pitcher[Cantorian, Boolean] { cantorian =>
  def asIterable: Iterable[Boolean] =
    new Iterable[Boolean] {
      override def iterator: Iterator[Boolean] =
        Iterator.iterate(
          cantorian
        ) {
          _.tail
        } map {
          _.head
        }
    }
}

object Cantorian {
  def apply(
    h: Boolean,
    t: => Cantorian
  ): Cantorian =
    new Cantorian {
      override val head: Boolean = h
      override def tail: Cantorian = t
    }

  def cycle(values: Boolean*): Cantorian =
    new Supplier[Cantorian] { supplier =>
      override def get: Cantorian =
        values
          .foldRight[Supplier[Cantorian]](
            supplier
          ) { (b: Boolean, c: Supplier[Cantorian]) => () => Cantorian(b, c.get) } get
    } get
}

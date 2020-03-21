package com.fdilke.bewl2.compacta

import java.util.function.{Consumer, Supplier}

import com.fdilke.bewl2.compacta.CantorianADTs.{GroundedTree, Pitcher}

import scala.language.postfixOps

object CantorianADTs {
  trait Pitcher[
    SELF <: Pitcher[SELF, T],
    T
  ] {
    val head: T
    def tail: SELF
  }

  sealed trait GroundedTree[T]
    extends Function[Cantorian, T]

  case class LeafNode[T](
    leaf: T
  ) extends GroundedTree[T] {
    def apply(cantorian: Cantorian): T =
      leaf
  }

  case class BranchNode[T](
    left: GroundedTree[T],
    right: GroundedTree[T]
  ) extends GroundedTree[T] {
    def apply(cantorian: Cantorian): T = {
      val side: GroundedTree[T] = (
        if (cantorian.head)
        left
      else
        right
      )

      val t: T = side.apply(
        cantorian.tail
      )

      t
    }
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
}

trait Cantorian extends
  Pitcher[Cantorian, Boolean] { cantorian =>
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
    new Supplier[Cantorian] {
      override def get: Cantorian =
        values.foldRight[() => Cantorian](
          () => get
        ) { (b: Boolean, c: () => Cantorian) =>
          () => new Cantorian {
            override val head: Boolean = b
            override def tail: Cantorian = c()
          }
        }()
    } get
}
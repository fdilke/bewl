package com.fdilke.bewl2.compacta

import java.util.function.{Consumer, Supplier}

import com.fdilke.bewl2.compacta.CantorianADTs.InexhaustibleIterator

import scala.language.postfixOps

object CantorianADTs {
  trait InexhaustibleIterator[T, U <: InexhaustibleIterator[T, U]] {
    val head: T
    def tail: U
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
}

trait Cantorian extends
  InexhaustibleIterator[Boolean, Cantorian] { cantorian =>
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
        values.foldLeft[() => Cantorian](
          () => get
        ) { (c: () => Cantorian, b: Boolean) =>
          () => new Cantorian {
            override val head: Boolean = b
            override def tail: Cantorian = c()
          }
        }()
    } get
}
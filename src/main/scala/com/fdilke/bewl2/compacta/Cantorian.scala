package com.fdilke.bewl2.compacta

import com.fdilke.bewl2.compacta.CantorianADTs.InexhaustibleIterator

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

  implicit class DeferredCantorian(
    deferred: () => Cantorian
  ) {
    def #::(head: Boolean): () => Cantorian = {
      println("VVV x: head = " + head)
      () => Cantorian(head, deferred())
    }
  }

  def cycle(values: Boolean*): Cantorian = {
    println("VVV entering the cycle")
    def loop(): Cantorian = {
      val hh: () => Cantorian =
        values.foldLeft[() => Cantorian](loop) { (c: (() => Cantorian), b: Boolean) =>
          val kk: () => Cantorian =
            b #:: new DeferredCantorian(c)
          kk
        }
      hh()
    }

    loop
  }
}
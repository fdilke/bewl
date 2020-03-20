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
        new Iterator[Boolean] {
          private var root: Cantorian =
            cantorian

          override def hasNext: Boolean =
            true

          override def next(): Boolean = {
            val theNext: Boolean =
              root.head
            root = root.tail
            theNext
          }
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

//  implicit def toDeferred](private val l:=> Cantorian): DeferredCantorian =
//    new DeferredCantorian(() => l)

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
      def ttt(c: () => Cantorian, b: Boolean): () => Cantorian =
        b #:: new DeferredCantorian(c)

      if (false) {
      val hh: () => Cantorian =
        values.foldLeft[() => Cantorian](loop) { (c: (() => Cantorian), b: Boolean) =>
          val kk: () => Cantorian =
            b #:: new DeferredCantorian(c)
          kk
        }

      (values.foldLeft[() => Cantorian](loop)(ttt)) ()
    }

      ttt(ttt(loop, false), true)()
//      hh()
    }
    //      cycle.foldLeft(loop) { _ #:: _ }
    println("VVV loop done")
    loop
  }
}
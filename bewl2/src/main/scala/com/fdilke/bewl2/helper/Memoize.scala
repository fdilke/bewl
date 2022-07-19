package com.fdilke.bewl2.helper

object Memoize {
//  def generic[A, B](
//    twoTypeFunc:
//  )

  trait Pig {
    def apply[X](x: X): X
  }

  def pig(
    func: Pig
  ): Unit =
    ()
//  def pig(
//    func: {
//      def apply[X](x: X): X
//    }
//  ): Unit =
//    ()

  def pigFn[X](x : X): X =
    x

  pig( new Pig {
    override def apply[X](x: X): X =
      pigFn[X](x)
    }
  )

  def apply[
    IN[_, _],
    OUT[_, _]
  ](fn: Memoizable2[IN, OUT]): Memoizable2[IN, OUT] =
    new Memoizable2[IN, OUT] {
      def apply[X, Y](
        input: IN[X, Y]
      ): OUT[X, Y] =
        fn[X, Y](
          input
        )
    }
}

trait Memoizable2[
  IN[_, _],
  OUT[_, _]
] {
  def apply[X, Y](
    input: IN[X, Y]
  ): OUT[X, Y]
}
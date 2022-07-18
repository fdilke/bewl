package com.fdilke.bewl2.experiments

object UnwiseScala3Experiments {

  trait Crappable[A, CTXT[_] /* <: Mappable[A, CTXT]] */ ] { self: CTXT[A] =>
    def map[B](f: A => B): CTXT[B]
  }

  def f[
    CTXT[A] <: Crappable[A, CTXT]
  ]: Int =
    2

  //    monad.eta
  //    val f: X => CTXT[X] =
  //      x => monad.eta(x)
  //    val g: X ~> X = f
  //    g

  // initial abortive experiments with type lambdas
  //  type M = [X, Y] =>> Map[Y, X]
  //  ({ type λ[T] = triadicMonoid.Action[T] })#λ
  //  type FTYPE = [H, X] =>> ( type { H[Y <: H[Y]] })#Y
  //  H[X <: H[X]]

}

package com.fdilke.bewl2

import scala.annotation.targetName

trait BaseTopos[
  SET[_],
  CTXT[_]: Monad
]:

  @targetName("topos arrow")
  type ~>[X, Y] = X => CTXT[Y]

  def equalArrows[X: SET, Y: SET](
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean
  
  def sanityTest[X: SET]: Unit
  def sanityTest[X: SET, Y: SET](f: X ~> Y): Unit

trait Topos[
  SET[_],
  CTXT[_]: Monad
] extends BaseTopos[SET, CTXT]:

  final val monad: Monad[CTXT] =
    implicitly[Monad[CTXT]]

  final inline def arrow[X: SET, Y: SET]( // necessary?
    f: X => CTXT[Y]
  ): X ~> Y =
    f

  final def id[X: SET]: X ~> X =
    monad.eta(_)
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

  implicit final class ArrowHelpers[X: SET, Y: SET](
    f: X ~> Y
  ):
    @targetName("functional equality")
    def =!=(
      f2: X ~> Y
    ): Boolean =
      equalArrows(f, f2)

    final def o[V: SET](
      f2: V ~> X
    ): V ~> Y =
      monad.mu[Y] compose monad.map(f) compose f2

object Topos:
  def apply[
    SET[_],
    CTXT[_]
  ](
   implicit topos: Topos[SET, CTXT]
  ): Topos[SET, CTXT] =
    topos

package com.fdilke.bewl2

import scala.annotation.targetName
import com.fdilke.bewl2.Mappable

trait BaseTopos[
  SET[_],
  CTXT[_] : Mappable
]:

//  type piggle = [Y] =>> CTXT[Y]
  @targetName("topos arrow")
  type ~>[X, Y] = (CTXT[X] => CTXT[Y])

  def equalArrows[X: SET, Y: SET](
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean
  
  def sanityTest[X: SET]: Unit
  def sanityTest[X: SET, Y: SET](f: X ~> Y): Unit

trait Topos[
  SET[_],
  CTXT[_]: Mappable
] extends BaseTopos[SET, CTXT]:

  final inline def arrow[X: SET, Y: SET]( // necessary?
    f: CTXT[X] => CTXT[Y]
  ): X ~> Y =
    f

  final def id[X: SET]: X ~> X =
    identity
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
      f compose f2

object Topos:
  def apply[
    SET[_],
    CTXT[A]: Mappable
  ](
   implicit topos: Topos[SET, CTXT]
  ): Topos[SET, CTXT] =
    topos

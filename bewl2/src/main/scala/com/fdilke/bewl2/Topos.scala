package com.fdilke.bewl2

import scala.annotation.targetName

trait BaseTopos[SET[_]]:

  type F[_]
  @targetName("topos arrow")
  type ~>[X, Y] = F[X] => F[Y]

  def equalArrows[X: SET, Y: SET](
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean

trait Topos[SET[_]] extends BaseTopos[SET]:

  final inline def arrow[X: SET, Y: SET]( // necessary?
    f: F[X] => F[Y]
  ): X ~> Y =
    f

  final def id[X: SET]: X ~> X =
    identity

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

    def o[V: SET](
      f2: V ~> X
    ): V ~> Y =
      f compose f2

object Topos:
  def apply[SET[_]: Topos]: Topos[SET] =
    implicitly[Topos[SET]]

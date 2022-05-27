package com.fdilke.bewl2

trait Topos[SET[_]]:

  type F[_]

// initial abortive experiments with type lambdas
//  type M = [X, Y] =>> Map[Y, X]
//  ({ type λ[T] = triadicMonoid.Action[T] })#λ
//  type FTYPE = [H, X] =>> ( type { H[Y <: H[Y]] })#Y
//  H[X <: H[X]]


object Topos:
  def apply[SET[_]: Topos]: Topos[SET] =
    implicitly[Topos[SET]]

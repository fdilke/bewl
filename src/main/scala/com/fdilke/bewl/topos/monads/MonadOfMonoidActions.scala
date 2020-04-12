package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.topos.algebra.AlgebraicStructures
import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}

trait MonadOfMonoidActions {
  Ɛ: BaseTopos with ToposStructures with AlgebraicStructures =>

  def monadOfActions[
    M <: ~
  ](
    monoid: Monoid[M]
  ) =
    new MonadOfActions(monoid)
      with CachingStrongMonad[
        ({ type λ[X <: ~] = X x M })#λ
      ]

  class MonadOfActions[
    M <: ~
  ](
    monoid: Monoid[M]
  ) extends StrongMonad[
      ({ type λ[X <: ~] = X x M })#λ
    ] {
    override def apply[X <: ~](
      dot: DOT[X]
    ) =
      new StrongMonad.At[
        ({ type λ[X <: ~] = X x M })#λ,
        X
      ] {
        override def tensorialStrength[
          Y <: ~
        ](
          dash: DOT[Y]
        ): >[x[X, x[Y, M]], x[x[X, Y], M]] = ???

        override lazy val free: DOT[X x M] =
          ???
        override lazy val eta: X > (X x M) =
          ???
        override lazy val mu: ((X x M) x M) > (X x M) =
          ???
      }

    override def map[
      X <: ~,
      Y <: ~
    ](
      arrow: X > Y
    ): (X x M) > (Y x M) =
      ???
  }
}

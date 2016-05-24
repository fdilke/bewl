package com.fdilke.bewl.topos.structures

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.BaseTopos

import scala.language.higherKinds

trait MonadCaching {
  Ɛ: BaseTopos with
    Monads with
    StrongMonads =>

  trait CachingMonad[
    M[X <: ~] <: ~
  ] extends Monad[M] {
    def atUncached[
    X <: ~
    ] (
      dot: DOT[X]
    ): At[X]

    final private val memoizedLocalValues =
      Memoize.generic.withLowerBound[
        ({type λ[X <: ~] = DOT[X]}) # λ,
        ({type λ[X <: ~] = At[X]}) # λ,
        ~
        ] (atUncached)

    override def apply[
    X <: ~
    ] (
      dot: DOT[X]
    ): At[X] =
      memoizedLocalValues(dot)
  }

  trait CachingStrongMonad[
  M[X <: ~] <: ~
  ] extends StrongMonad[M] {
    def atUncached[
    X <: ~
    ] (
      dot: DOT[X]
    ): At[X]

    final private val memoizedLocalValues =
      Memoize.generic.withLowerBound[
        ({type λ[X <: ~] = DOT[X]}) # λ,
        ({type λ[X <: ~] = At[X]}) # λ,
        ~
        ] (atUncached)

    override def apply[
    X <: ~
    ] (
      dot: DOT[X]
    ): At[X] =
      memoizedLocalValues(dot)
  }
}

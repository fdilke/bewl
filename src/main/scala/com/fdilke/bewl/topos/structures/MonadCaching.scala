package com.fdilke.bewl.topos.structures

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.BaseTopos

trait MonadCaching {
  Ɛ: BaseTopos with Monads with StrongMonads =>

  trait CachingMonad[
    M[X <: ~] <: ~
  ] extends Monad[M] {
    final private val memoizedLocalValues =
      Memoize.generic.withLowerBound[
        ({ type λ[X <: ~] = DOT[X] })#λ,
        ({ type λ[X <: ~] = Monad.At[M, X] })#λ,
        ~
      ](super.apply)

    abstract override def apply[
      X <: ~
    ](
      dot: DOT[X]
    ) =
      memoizedLocalValues(dot)
  }

  trait CachingStrongMonad[
    M[X <: ~] <: ~
  ] extends StrongMonad[M] {
    final private val memoizedLocalValues =
      Memoize.generic.withLowerBound[
        ({ type λ[X <: ~] = DOT[X] })#λ,
        ({ type λ[X <: ~] = StrongMonad.At[M, X] })#λ,
        ~
      ](super.apply)

    abstract override def apply[
      X <: ~
    ](
      dot: DOT[X]
    ) =
      memoizedLocalValues(dot)
  }
}

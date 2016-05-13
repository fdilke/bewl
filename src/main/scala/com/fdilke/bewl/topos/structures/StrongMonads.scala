package com.fdilke.bewl.topos.structures

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.{ToposStructures, BaseTopos}
import org.scalatest.Matchers._

import scala.language.{higherKinds, reflectiveCalls}

trait StrongMonads {
  Ɛ: BaseTopos with ToposStructures =>

  trait StrongMonad[
    M[X <: ~] <: ~
  ] extends Monad[M] {

    final private val memoizedLocalValues =
      Memoize.generic.withLowerBound[
        ({type λ[X <: ~] = DOT[X]}) # λ,
        At,
        ~
      ] (atUncached)

    override def apply[
      X <: ~
    ] (
      dot: DOT[X]
    ): At[X] =
      memoizedLocalValues(dot)

    override def atUncached[
      X <: ~
    ] (
      dot: DOT[X]
    ): At[X]

    trait At[
      X <: ~
    ] extends super.At[X] {
      def tensorialStrength[
        Y <: ~
      ](
        dot: DOT[Y]
      ):
        X x M[Y] > M[X x Y]
    }
//    def sanityTest3
  }
}

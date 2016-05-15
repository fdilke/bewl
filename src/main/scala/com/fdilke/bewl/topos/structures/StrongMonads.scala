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

    def tensorialStrength[
      A <: ~,
      B <: ~
    ](
      a: DOT[A],
      b: DOT[B]
    ) =
      apply(
        a
      ).tensorialStrength(
        b
      )

    // Verify the axioms for a strong monad :
    // https://en.wikipedia.org/wiki/Strong_monad

    def sanityTest3[
      A <: ~
    ](
      a: DOT[A]
    ) {
      val stIA =
        tensorialStrength(I, a)

      map((I x a).π1) o stIA shouldBe
        (I x apply(a).free).π1
    }

    def sanityTest4[
      A <: ~,
      B <: ~
    ](
      a: DOT[A],
      b: DOT[B]
    ) {
      val stAB =
        tensorialStrength(a, b)
      val axb = a x b

      stAB o (axb.π0 x (apply(b).eta o axb.π1)) shouldBe
        apply(axb).eta
    }

    def sanityTest5[
      A <: ~,
      B <: ~,
      C <: ~
    ](
      a: DOT[A],
      b: DOT[B],
      c: DOT[C]
    ) {
      val tc = apply(c).free
      val a_btc = a x (b x tc)
      val stBC = tensorialStrength(b, c)

      tensorialStrength(a, b x c) o
        (a_btc.π0 x (stBC o a_btc.π1)) o
        associator(a, b, tc) shouldBe (
          map(
            associator(a, b, c)
          ) o
          tensorialStrength(a x b, c)
        )
    }
  }
}

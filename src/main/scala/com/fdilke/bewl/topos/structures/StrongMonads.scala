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
      val bxc = b x c
      val a_btc = a x (b x apply(c).free)
      val ab_tc = (a x b) x apply(c).free

      // TODO: factor out calculation of associator
      val associator =
        ((a x b).π0 o ab_tc.π0) x (
          ((a x b).π1 o ab_tc.π0) x (
            ab_tc.π1
          )
        )
      associator shouldBe 'iso

      val stBC = tensorialStrength(b, c)

      a_btc.π0

//      val x = ???
//
//      tensorialStrength(a, bxc) o
//        (a_btc.π0 x (stBC o a_btc.π1)) o
//        associator shouldBe x

//      ???
    }
  }
}

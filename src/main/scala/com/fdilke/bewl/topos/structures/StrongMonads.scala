package com.fdilke.bewl.topos.structures

import com.fdilke.bewl.topos.enrichment.MonadicPlumbing
import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}
import org.scalatest.Matchers._

import scala.language.{higherKinds, reflectiveCalls}

trait StrongMonads {
  Ɛ: BaseTopos with ToposStructures with MonadicPlumbing =>

  trait StrongMonad[
    M[X <: ~] <: ~
  ] extends Monad[M] {

    def apply[
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
        dash: DOT[Y]
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

    def cotensorialStrength[
      X <: ~,
      Y <: ~
    ](
      x: DOT[X],
      y: DOT[Y]
    ): M[X] x Y > M[X x Y] =
      map(twist(y, x)) o
        tensorialStrength(y, x) o
        twist(T(x), y)

    def functorialStrength[
      X <: ~,
      Y <: ~
    ](
      x: DOT[X],
      y: DOT[Y]
    ): (X → Y) > (
      M[X] → M[Y]
      ) = (
      T(x) > T(y)
      ).transpose(
      x > y
    ) { (x2y, mx) =>
      map(
        (x > y).evaluation.arrow
      )(
        tensorialStrength(
          x > y,
          x
        )(
          (
            (x > y) x T(x)
            ).pair(
            x2y,
            mx
          )
        )
      )
    }

    private def T[
      A <: ~
    ](
      a: DOT[A]
    ) =
      apply(a).free

    private def η[
      A <: ~
    ](
      a: DOT[A]
    ) =
      apply(a).eta

    private def μ[
      A <: ~
    ](
      a: DOT[A]
    ) =
      apply(a).mu

    // Verify the axioms for a strong monad :
    // https://en.wikipedia.org/wiki/Strong_monad

    def sanityTest3[
      A <: ~
    ](
      a: DOT[A]
    ) =
      map(
        I -* a
      ) o tensorialStrength(
        I,
        a
      ) shouldBe
        (I -* T(a))

    def sanityTest4[
      A <: ~,
      B <: ~
    ](
      a: DOT[A],
      b: DOT[B]
    ) =
      tensorialStrength(a, b) o (
        (a *- b) x (
          η(b) o (a -* b)
        )
      ) shouldBe
        η(a x b)

    def sanityTest5[
      A <: ~,
      B <: ~,
      C <: ~
    ](
      a: DOT[A],
      b: DOT[B],
      c: DOT[C]
    ) {
      val a_btc = a x (b x T(c))
      val stBC = tensorialStrength(b, c)

      tensorialStrength(a, b x c) o
        (a_btc.π0 x (stBC o a_btc.π1)) o
        associator(a, b, T(c)) shouldBe (
          map(
            associator(a, b, c)
          ) o
          tensorialStrength(a x b, c)
        )
    }

    def sanityTest6[
      A <: ~,
      B <: ~
    ](
      a: DOT[A],
      b: DOT[B]
    ) {
      val stAB = tensorialStrength(a, b)
      val stATB = tensorialStrength(a, T(b))
      val axttb = a x T(T(b))

      stAB o (axttb.π0 x (μ(b) o axttb.π1)) shouldBe (
        μ(a x b) o map(stAB) o stATB
      )
    }
  }
}

package com.fdilke.bewl.topos

import com.fdilke.bewl.helper.Memoize
import org.scalatest.Matchers
import Matchers._
import scala.language.higherKinds

trait Monads[~] { topos: BaseTopos[~] =>

  trait Monad[M[X <: ~] <: ~] {
    final private val memoizedLocalValues =
      Memoize.generic.withLowerBound[
        ({type λ[X <: ~] = DOT[X]}) # λ,
        At,
        ~
      ] (atUncached _)

    final def apply[X <: ~](dot: DOT[X]): At[X] =
      memoizedLocalValues(dot)

    def atUncached[X <: ~](dot: DOT[X]): At[X]
    def map[X <: ~, Y <: ~](arrow: X > Y): M[X] > M[Y]

    trait At[X <: ~] {
      val free: DOT[M[X]]
      val eta: X > M[X]
      val mu: M[M[X]] > M[X]

      def sanityTest = {
        mu o map(eta) shouldBe free.identity
        mu o apply(free).eta shouldBe free.identity
      }

      def sanityTest2 = {
        mu o map(mu) shouldBe (mu o apply(free).mu)
      }
    }

    case class Algebra[X <: ~](structure: M[X] > X){
      lazy val carrier = structure.target
      lazy val local = apply(carrier)

      def sanityTest =
        (structure o local.eta) shouldBe carrier.identity
      def sanityTest2 =
        (structure o map(structure)) shouldBe (structure o local.mu)
    }
  }
}

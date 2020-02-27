package com.fdilke.bewl2.topos

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl2.topos.FunctionalPlumbing.{CharacteristicArrow, EqualizerReceiver}
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.Function.tupled
import scala.language.postfixOps

trait Topos[DOT[_]] { topos =>
  val name: String = getClass.getSimpleName

  type >[A, B] <: A => B
  type Ω

  implicit val initial : DOT[Void]
  def from0[S: DOT]: Void => S

  implicit val terminator : DOT[Unit]
  def to1[S: DOT]: S => Unit

  implicit val omega : DOT[Ω]
  val truth: Unit => Ω
  def chi[S: DOT, T: DOT](
    monic: S => T
  ): CharacteristicArrow[DOT, S, T, Ω]

  def sanityTest[S: DOT]: Unit
  def sanityTest[S: DOT, T:DOT](arrow: S => T): Unit

  def toTrue[S: DOT]: S => Ω =
    truth o to1[S]

  def compareFunctions[S:DOT, T:DOT](func: S=> T, func2: S => T): Boolean
  def functionAsString[S: DOT, T: DOT](arrow: S => T): String
  def productUncached[A : DOT, B : DOT]: DOT[(A, B)]
  def exponentialUncached[A : DOT, B : DOT]: DOT[A > B]

  def transpose[A:DOT, B:DOT, C:DOT](
    arrow: (A, B) => C
  ): A => (B > C)

  def equalize[S:DOT, T:DOT, X](
    func1: S => T,
    func2: S => T
  ): EqualizerReceiver[DOT, S, X] => X

  // TODO: put this helper code in a separate class

  @inline implicit class RichFunction[S: DOT, T:DOT] (
    function: S => T
  ) {
    @inline final def =?=(function2: S => T): Boolean =
      compareFunctions(function, function2)

    def shouldBeFn: Matcher[S => T] =
      function2 =>
        MatchResult(
          =?=(function2),
          "functions do not match",
          "functions match"
        )

    def shouldNotBeFn: Matcher[S => T] =
      Matchers.not(shouldBeFn)

    @inline final def ?=[X](function2: S => T): EqualizerReceiver[DOT, S, X] => X =
      equalize(function, function2)

    @inline final def o[R: DOT](function2: R => S): R => T =
      function compose function2

    @inline final def x[U: DOT](function2: S => U): S => (T, U) =
      s => (function(s), function2(s))

    @inline final def sanityTest =
      topos.sanityTest(function)

    @inline final def chi: CharacteristicArrow[DOT, S, T, Ω] =
      topos.chi(function)

    @inline final def source: DOT[S] =
      dot[S]
    @inline final def target: DOT[T] =
      dot[T]

    @inline final def name: Unit => (S > T) =
      { (_: Unit, s: S) =>
        function(s)
      } transpose
  }

  @inline implicit class RichBiFunction[S: DOT, T:DOT, U: DOT] (
   function: (S, T) => U
 ) {
    @inline final def =?=(function2: (S, T) => U): Boolean =
      topos.transpose(function) =?= topos.transpose(function2)

    @inline final def transpose: S => (T > U) =
      topos.transpose(function)
  }

  // anticipate these will not be used very much...
  // as it's all baked into the types and thoughtcrime is impossible. remove?
  @inline final def dot[S:DOT]: DOT[S] =
    implicitly[DOT[S]]

  final def id[S: DOT]: S => S =
    identity

  final private class DotExtras[A: DOT] {
    final private def makeProduct[B](
      dot: DOT[B]
    ): DOT[(A, B)] = {
      implicit val theDot: DOT[B] = dot
      productUncached[A, B]
    }

    final private val memoizedProduct =
      Memoize.generic[
        DOT,
        ({ type λ[B] = DOT[(A, B)]}) # λ,
      ] (makeProduct)

    def productDot[B : DOT]: DOT[(A, B)] =
      memoizedProduct(dot[B])

    final private def makeExponential[B](
      dot: DOT[B]
    ): DOT[A > B] = {
      implicit val theDot: DOT[B] = dot
      exponentialUncached[A, B]
    }

    final private val memoizedExponential =
      Memoize.generic[
        DOT,
        ({ type λ[B] = DOT[A > B]}) # λ,
      ] (makeExponential)

    def exponential[B : DOT]: DOT[A > B] =
      memoizedExponential(dot[B])

    lazy val `∀`: ((A > Ω) => Ω) =
      toTrue[A].name.chi.chi
  }

  final private def makeDotExtras[A](
    dot: DOT[A]
  ): DotExtras[A] = {
    implicit val theDot: DOT[A] = dot
    new DotExtras[A]
  }

  final private val memoizedExtras =
    Memoize.generic[
      DOT,
      DotExtras
    ] (makeDotExtras)

  final private def extras[A: DOT]: DotExtras[A] =
    memoizedExtras(dot[A])

  implicit def productDot[A: DOT, B: DOT]: DOT[(A, B)] =
    extras[A].productDot[B]

  implicit def exponentialDot[A: DOT, B: DOT]: DOT[A > B] =
    extras[A].exponential[B]

  @inline final def ∀[A: DOT]: (A > Ω) => Ω =
    extras[A].`∀`

  // Projection operators
  def π0[A : DOT, B : DOT]: ((A, B)) => A =
    tupled { (a, b) => a }
  def π1[A : DOT, B : DOT]: ((A, B)) => B =
    tupled { (a, b) => b }

  // TODO: this works but is monstrous. Remedy is to abolish
  //  materialized products in favour of multiary plumbing?
  def π0[A: DOT, B: DOT, C: DOT]: (((A, B), C)) => A = {
    val k: (((A, B), C)) => (A, B) = π0[(A, B), C]
    val l: ((A, B)) => A = π0[A, B]

    val t: (((A, B), C)) => A = {
      (abc: ((A, B), C)) =>
        val ab: (A, B) = k(abc)
        val a: A = l(ab)
        a
    }
    t
  }
//    π0[A, B] o π0[(A, B), C]

  //  implicit def multiFunction2function[A: DOT, B: DOT, C: DOT](
//    mf: (A, B) => C
//  ): ((A, B)) => C =
//    Function.tupled(mf)
}

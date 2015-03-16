package com.fdilke.bewl.diagrammatic

import com.fdilke.bewl.diagrammatic.algebra.{DiagrammaticAlgebra, DiagrammaticAlgebraicLaws, DiagrammaticAlgebraicStructures, TruthObject}
import com.fdilke.bewl.helper.Memoize
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.existentials

trait DiagrammaticTopos extends BaseDiagrammaticTopos with DiagrammaticAlgebra with DiagrammaticAlgebraicLaws with
  DiagrammaticAlgebraicStructures with TruthObject

trait BaseDiagrammaticTopos {
  type DOT[P] <: Dot[P]
  type ARROW[P, Q] <: Arrow[P, Q]

  type EQUALIZER_SOURCE[M]
  type TERMINAL
  type OMEGA

  val I: DOT[TERMINAL]
  val omega: DOT[OMEGA]
  val truth: ARROW[TERMINAL, OMEGA]

  type Operator[X] = ARROW[Power[X], X]

  trait Dot[X] { dot: DOT[X] =>
    def identity: ARROW[X, X]
    def toI: ARROW[X, TERMINAL]
    def multiply[Y](that: DOT[Y]): Biproduct[X, Y]
    val globals: Traversable[ARROW[TERMINAL, X]]

    private val memoizedBiproduct = {
      type BIPRODUCT[Y] = Biproduct[X, Y]
      def biproduct[Y](that: DOT[Y]): BIPRODUCT[Y] = this multiply that
      Memoize.generic[DOT, BIPRODUCT](biproduct)
    }

    final def *[Y](that: DOT[Y]): Biproduct[X, Y] = memoizedBiproduct(that)
    final def x[Y](that: DOT[Y]) = (this * that).product
    def exponential[S](that: DOT[S]): Exponential[S, X]
    private val memoizedExponential = {
      type EXPONENTIAL[S] = Exponential[S, X]
      def exponential[S](that: DOT[S]): EXPONENTIAL[S] = this exponential that
      Memoize.generic[DOT, EXPONENTIAL](exponential)
    }

    final def A[S](that: DOT[S]): Exponential[S, X] = memoizedExponential(that)
    final def ^[S](that: DOT[S]): DOT[S => X] = (this A that).exponentDot
    private val memoizedPower = {
      def power[T](exponent: Int) = this toPower exponent
      Memoize(power)
    }
    final def A(exponent: Int) = memoizedPower(exponent)
    final def ^(exponent: Int): DOT[Power[X]] = (this A exponent).power
    final def toPower(exponent: Int): IntegerPower[X] = exponent match {
      case 0 => IntegerPower[X](I, Seq.empty)
      case 1 => IntegerPower[X](this, Seq(this.identity))
      case _ =>
        val xN_1 = this A (exponent - 1)
        val product = this * xN_1.power
        IntegerPower[X](product.product,
          product.leftProjection +: xN_1.projection.map(_(product.rightProjection))
        )
    }

    final def >>[T](target: DOT[T]): Traversable[ARROW[X, T]] = {
      val exponential = target.A(this)
      exponential.exponentDot.globals.map { global =>
        exponential.evaluation.arrow(
          global(toI) x identity
        )}}

    def sanityTest: Unit
  }

  trait Arrow[X, Y] {
    val source: DOT[X]
    val target: DOT[Y]

    def apply[W](arrow: ARROW[W, X]): ARROW[W, Y]

    final def x[Z](that: ARROW[X, Z]): ARROW[X, (Y, Z)] = (this.target * that.target).
      multiply(this.asInstanceOf[ARROW[X, Y]], that)

    def ?=(that: ARROW[X, Y]): Equalizer[X]

    val chi: Characteristic[X, Y]

    def name = transpose(source, target, BiArrow(I, source, this((I * source).rightProjection)))

    def sanityTest: Unit
  }

  trait Biproduct[X, Y] {
    val product: DOT[(X, Y)]
    val leftProjection: ARROW[(X, Y), X]
    val rightProjection: ARROW[(X, Y), Y]

    def multiply[W](leftArrow: ARROW[W, X],
                    rightArrow: ARROW[W, Y]): ARROW[W, (X, Y)]
  }

  trait Exponential[S, T] {
    val exponentDot: DOT[S => T]
    val evaluation: BiArrow[S => T, S, T]

    def transpose[W](multiArrow: BiArrow[W, S, T]): ARROW[W, S => T]
  }

  // Should this really be part of the base API? Needed for Elemental/Wrapping
  def buildArrow[S, T](source: DOT[S], target: DOT[T], f: S => T): ARROW[S, T]

  case class BiArrow[L, R, T](left: DOT[L], right: DOT[R], arrow: ARROW[(L, R), T]) {
    def apply[S](leftArrow: ARROW[S, L], rightArrow: ARROW[S, R]): ARROW[S, T] =
      arrow(leftArrow x rightArrow)
  }

  abstract class Characteristic[X, Y] {
    val arrow: ARROW[Y, OMEGA]

    def restrict[W](arrow: ARROW[W, Y]): ARROW[W, X]
  }

  trait Equalizer[M] {
    val equalizerSource: DOT[EQUALIZER_SOURCE[M]]
    val equalizer: ARROW[EQUALIZER_SOURCE[M], M]

    def restrict[S](equalizingArrow: ARROW[S, M]): ARROW[S, EQUALIZER_SOURCE[M]]
  }

  case class IntegerPower[X](_power: Dot[_], _projection: Seq[ARROW[_, _]]) {
    def power = _power.asInstanceOf[DOT[Power[X]]]

    def projection = _projection.asInstanceOf[Seq[Operator[X]]]
  }

  object IntegerPower {
    def multiply[S, X](source: DOT[S], arrows: ARROW[S, X]*): ARROW[S, Power[X]] =
      (arrows size match {
        case 0 => source.toI
        case 1 => arrows(0)
        case _ => arrows.head x multiply(source, arrows.tail: _*)
      }).asInstanceOf[ARROW[S, Power[X]]]
  }

  class Power[X] // just a marker, for now - will have methods as part of the DSL?

  // Helper methods for biproducts
  def leftProjection[X, Y](x: DOT[X], y: DOT[Y]) = (x * y).leftProjection

  def rightProjection[X, Y](x: DOT[X], y: DOT[Y]) = (x * y).rightProjection

  // Helper methods for triproducts (this could obviously be extended)
  def leftProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = (x * y).leftProjection(((x x y) * z).leftProjection)

  def midProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = (x * y).rightProjection(((x x y) * z).leftProjection)

  def rightProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = ((x x y) * z).rightProjection

  // Helper methods for exponentials
  def evaluation[S, T](s: DOT[S], t: DOT[T]): BiArrow[S => T, S, T] =
    (t A s).evaluation

  def transpose[S, T, W](s: DOT[S], t: DOT[T], multiArrow: BiArrow[W, S, T]): ARROW[W, S => T] =
    (t A s).transpose(multiArrow)

  // Helper methods for integer powers
  def projection[X](base: DOT[X], exponent: Int, index: Int): Operator[X] =
    (base A exponent).projection(index)
}


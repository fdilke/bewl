package com.fdilke.bewl

import com.fdilke.bewl.algebra.AlgebraicStructure.Signature
import com.fdilke.bewl.algebra.{Operator}
import com.fdilke.bewl.helper.ResultStore
import Function.tupled

trait Topos {
  type DOT[P] <: Dot[P]
  type ARROW[P, Q] <: Arrow[P, Q]
  type BIPRODUCT[P, Q] <: Biproduct[P, Q]
  type EXPONENTIAL[P, Q] <: Exponential[P, Q]
  type EQUALIZER[M, T] <: Equalizer[M, T]

  type EQUALIZER_SOURCE[M, T]
  type TERMINAL
  type OMEGA

  val I: DOT[TERMINAL]
  val omega: DOT[OMEGA]
  val truth: ARROW[TERMINAL, OMEGA]

  trait Dot[X] {
    def identity: ARROW[X, X]

    def toI: ARROW[X, TERMINAL]

    def multiply[Y](that: DOT[Y]): BIPRODUCT[X, Y]

    final def *[Y](that: DOT[Y]) = standardProducts(
      (this.asInstanceOf[DOT[Any]],
       that.asInstanceOf[DOT[Any]])).asInstanceOf[BIPRODUCT[X, Y]]

    final def x[Y](that: DOT[Y]) = (this * that).product

    def exponential[S](that: DOT[S]): EXPONENTIAL[S, X]

    final def A[S](that: DOT[S]) = standardExponentials(
      (this.asInstanceOf[DOT[Any]],
        that.asInstanceOf[DOT[Any]])).asInstanceOf[EXPONENTIAL[S, X]]

    final def ^[S](that: DOT[S]): DOT[S => X] = (this A that).exponentDot

    final def A[S](exponent: Int) = standardPowers(
      this.asInstanceOf[DOT[Any]],
      exponent).asInstanceOf[IntegerPower[X]]

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
  }

  trait Arrow[X, Y] {
    val source: DOT[X]
    val target: DOT[Y]

    def apply[W](arrow: ARROW[W, X]): ARROW[W, Y]

    final def x[Z](that: ARROW[X, Z]) = (this.target * that.target).
      multiply(this.asInstanceOf[ARROW[X, Y]], that)

    def ?=(that: ARROW[X, Y]): EQUALIZER[X, Y]
    val chi: Characteristic[X, Y]
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

  case class BiArrow[L, R, T](left: DOT[L], right: DOT[R], arrow: ARROW[(L, R), T])
  abstract class Characteristic[X, Y] {
    val arrow: ARROW[Y, OMEGA]
    def restrict[W](arrow: ARROW[W, Y]): ARROW[W, X]
  }

  trait Equalizer[M, T] {
    val equalizerSource: DOT[EQUALIZER_SOURCE[M, T]]
    val equalizer: ARROW[EQUALIZER_SOURCE[M, T], M]
    def restrict[S](equalizingArrow: ARROW[S, M]): ARROW[S, EQUALIZER_SOURCE[M, T]]
  }

  case class IntegerPower[X](_power: Dot[_], _projection: Seq[ARROW[_, _]]) {
    def power = _power.asInstanceOf[DOT[Power[X]]]
    def projection = _projection.asInstanceOf[Seq[ARROW[Power[X], X]]]
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

  private val standardProducts = new ResultStore[(DOT[Any], DOT[Any]), BIPRODUCT[Any, Any]](tupled {
    (x, y) => x multiply y
  })

  private val standardExponentials = new ResultStore[(DOT[Any], DOT[Any]), EXPONENTIAL[Any, Any]](tupled {
    (x, y) => x exponential y
  })

  private val standardPowers = new ResultStore[(DOT[Any], Int), IntegerPower[Any]](tupled {
    (x, n) => x toPower n
  })

  // Helper methods for biproducts
  def leftProjection[X, Y](x: DOT[X], y: DOT[Y]) = (x * y).leftProjection
  def rightProjection[X, Y](x: DOT[X], y: DOT[Y]) = (x * y).rightProjection

  // Helper methods for triproducts (this could obviously be extended)
  def leftProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = (x * y).leftProjection(((x x y) * z).leftProjection)
  def midProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = (x * y).rightProjection(((x x y) * z).leftProjection)
  def rightProjection[X, Y, Z](x: DOT[X], y: DOT[Y], z: DOT[Z]) = ((x x y) * z).rightProjection

  // Helper methods for exponentials
  def evaluation[S, T](s: DOT[S], t: DOT[T]): BiArrow[S=>T, S, T] =
    (t A s).evaluation
  def transpose[S, T, W](s: DOT[S], t: DOT[T], multiArrow: BiArrow[W, S, T]): ARROW[W, S => T] =
    (t A s).transpose(multiArrow)

  // Helper methods for integer powers
  def projection[X](base: DOT[X], exponent: Int, index: Int): ARROW[Power[X], X] =
    (base A exponent).projection(index)

  // Machinery for constructing and verifying algebraic structures with laws (varieties)
  // TODO: is there a way for this stuff to exist outside Topos?

  trait Law {
    val arity: Int
    def apply[S, X](root: DOT[S], variables: Seq[ARROW[S, X]], ops: Map[Operator, AlgebraicArrow[X]])
  }

  object Law {
    def commutative(operator: Operator) = new Law {
      val arity = 2
      override def apply[S, X](root: DOT[S], variables: Seq[ARROW[S, X]], ops: Map[Operator, AlgebraicArrow[X]]) = {
        ops.get(operator).map { op =>
          val Seq(x, y) = variables
          if (op(root, x, y) != op(root, y, x)) {
            throw new IllegalArgumentException(s"Commutative law failed for operator $operator")
          }
        } orElse {
          throw new IllegalArgumentException(s"Operator $operator not defined")
        }
      } // TODO: can make this less of a rat's breakfast, not have to pass in the root?
    }
  }

  case class AlgebraicArrow[X](val arrow: ARROW[Power[X], X]) {
    def apply[S](source: DOT[S], variables: ARROW[S, X]*): ARROW[S, X] =
      arrow(IntegerPower.multiply(source, variables:_*))
  }

  class AlgebraicStructure[X](val carrier: DOT[X], val signature: Signature, val ops: Map[Operator, AlgebraicArrow[X]], val laws: Seq[Law]) {
    def verify = laws.map { law =>
      def power = carrier A law.arity
      law(power.power, power.projection, ops)
    }
  }
}


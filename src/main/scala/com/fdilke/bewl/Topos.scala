package com.fdilke.bewl

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

    final def ^[S](that: DOT[S]): DOT[S => X] = (this A that).evaluation.left
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

  private val standardProducts = new ResultStore[(DOT[Any], DOT[Any]), BIPRODUCT[Any, Any]](tupled {
    (x, y) => x multiply y
  })

  private val standardExponentials = new ResultStore[(DOT[Any], DOT[Any]), EXPONENTIAL[Any, Any]](tupled {
    (x, y) => x exponential y
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
}


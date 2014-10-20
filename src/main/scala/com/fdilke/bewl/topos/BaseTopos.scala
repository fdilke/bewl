package com.fdilke.bewl.topos

import Function.tupled

trait Topos extends BaseTopos with SafeAlgebra

trait BaseTopos {
  type ELEMENT
  type STAR[S <: ELEMENT] <: Star[S]
  type QUIVER[S <: ELEMENT, T <: ELEMENT] <: Quiver[S, T]

  type >[T <: ELEMENT, U <: ELEMENT] = (T => U) with ELEMENT
  type x[T <: ELEMENT, U <: ELEMENT] = (T, U) with ELEMENT

  type UNIT <: ELEMENT
  val I : STAR[UNIT]

  type TRUTH <: ELEMENT
  val omega: STAR[TRUTH]
  val truth: QUIVER[UNIT, TRUTH]

  type EXPONENTIAL[S <: ELEMENT, T <: ELEMENT] = ExponentialStar[S, T] with STAR[S > T]
  trait ExponentialStar[S <: ELEMENT, T <: ELEMENT] { star: STAR[S > T] =>
    val source: STAR[S]
    val target: STAR[T]
    def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, S, T]): QUIVER[R, S > T]
    final def evaluation: BiQuiver[S > T, S, T] =
      (this x source).biQuiver(target) { (f, s) => f(s) }
  }

  type BIPRODUCT[L <: ELEMENT, R <: ELEMENT] = BiproductStar[L, R] with STAR[L x R]
  trait BiproductStar[L <: ELEMENT, R <: ELEMENT] { star: STAR[L x R] =>
    val left: STAR[L]
    val right: STAR[R]
    def pair(l: L, r: R): L x R
    final lazy val π0 = star(left) { _._1 }
    final lazy val π1 = star(right) { _._2 }
    final def biQuiver[T <: ELEMENT](
      target: STAR[T]
      ) (
      bifunc: (L, R) => T
      ) : BiQuiver[L, R, T] =
      BiQuiver(this, this(target) (
        tupled[L,R,T](bifunc)
      ))
    }

  type EQUALIZER[S <: ELEMENT] = EqualizingStar[S] with STAR[S]
  trait EqualizingStar[S <: ELEMENT] { star: STAR[S] =>
    val equalizerTarget: STAR[S]
    val inclusion: QUIVER[S, S]
    def restrict[R <: ELEMENT](quiver: QUIVER[R, S]): QUIVER[R, S]
  }

  trait Star[S <: ELEMENT] { self: STAR[S] =>
    final lazy val identity: QUIVER[S, S] = this(self) { s => s }
    val toI: QUIVER[S, UNIT]
    def x[T <: ELEMENT](that: STAR[T]): BIPRODUCT[S, T]
    def >[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[S, T]
    def apply[T <: ELEMENT](target: STAR[T])(f: S => T) : QUIVER[S, T]
    def sanityTest
  }

  trait Quiver[S <: ELEMENT, T <: ELEMENT] {
    val source: STAR[S]
    val target: STAR[T]
    val chi: QUIVER[T, TRUTH]

    def apply(s: S): T
    def ?=(that: QUIVER[S, T]): EQUALIZER[S]
    def o[R <: ELEMENT](that: QUIVER[R, S]) : QUIVER[R, T]
    final def x[U <: ELEMENT](that: QUIVER[S, U]): QUIVER[S, x[T, U]] = {
      val product = this.target x that.target
      source(product) {
        s => product.pair(this(s), that(s))
      }}

    def \[U <: ELEMENT](monic: QUIVER[U, T]) : QUIVER[S, U]
    def sanityTest
  }

  case class BiQuiver[
    L <: ELEMENT,
    R <: ELEMENT,
    T <: ELEMENT](
    product: BIPRODUCT[L, R],
    quiver: QUIVER[L x R, T]) {
    def apply(l: L, r: R): T = quiver(product.pair(l, r))
  }

  // Helper methods for triproducts (this could obviously be extended).
  // TODO: Only used in tests: delete when there is a satisfactory replacement (multiproducts)
  def leftProjection[X <: ELEMENT, Y <: ELEMENT, Z <: ELEMENT](
    x: STAR[X], y: STAR[Y], z: STAR[Z]
  ) : QUIVER[X x Y x Z, X] =
    (x x y).π0 o (x x y x z).π0

  def midProjection[X <: ELEMENT, Y <: ELEMENT, Z <: ELEMENT](
   x: STAR[X], y: STAR[Y], z: STAR[Z]
  ) : QUIVER[X x Y x Z, Y] =
    (x x y).π1 o (x x y x z).π0

  def rightProjection[X <: ELEMENT, Y <: ELEMENT, Z <: ELEMENT](
   x: STAR[X], y: STAR[Y], z: STAR[Z]
  ) : QUIVER[X x Y x Z, Z] =
    (x x y x z).π1
}

trait Wrappings { topos: BaseTopos =>
  type PRESTAR[T]
  type PREQUIVER[S, T]
  type WRAPPER[T] <: ELEMENT

  def star[T](input: PRESTAR[T]) : STAR[WRAPPER[T]]
  def quiver[S, T](connector: PREQUIVER[S, T]) : QUIVER[WRAPPER[S], WRAPPER[T]]
  def functionAsQuiver[S, T](source: STAR[WRAPPER[S]], target: STAR[WRAPPER[T]], f: S => T): QUIVER[WRAPPER[S], WRAPPER[T]]
  def bifunctionAsBiQuiver[L, R, T] (
      left: STAR[WRAPPER[L]],
      right: STAR[WRAPPER[R]],
      target: STAR[WRAPPER[T]],
      bifunc: (L, R) => T
   ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]]
}
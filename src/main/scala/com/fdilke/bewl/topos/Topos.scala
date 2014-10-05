package com.fdilke.bewl.topos

trait Topos {
  type ELEMENT
  type STAR[S <: ELEMENT] <: Star[S]
  type QUIVER[S <: ELEMENT, T <: ELEMENT] <: Quiver[S, T]

  type x[T <: ELEMENT, U <: ELEMENT] <: xI[T, U] with ELEMENT

  trait xI[T <: ELEMENT, U <: ELEMENT] {
    val left: T
    val right: U
  }

  type UNIT <: ELEMENT
  val I : STAR[UNIT]

  trait Star[S <: ELEMENT] {
    val identity: QUIVER[S, S]
    val toI: QUIVER[S, UNIT]
    def x[T <: ELEMENT](that: STAR[T]): STAR[S x T]
    def sanityTest

    def apply[T <: ELEMENT](target: STAR[T])(f: S => T) : QUIVER[S, T]
  }

  trait Quiver[S <: ELEMENT, T <: ELEMENT] {
    val source: STAR[S]
    val target: STAR[T]

    def o[R <: ELEMENT](that: QUIVER[R, S]) : QUIVER[R, T]
    def x[U <: ELEMENT](that: QUIVER[S, U]): QUIVER[S, T x U]
    def sanityTest
  }

  // TODO extras - separate into a trait?

  // Helper methods for biproducts: Compactify?

  def leftProjection[A <: ELEMENT, B <: ELEMENT](left: STAR[A], right: STAR[B]) : QUIVER[A x B, A] =
    (left x right)(left) { _.left }

  def rightProjection[A <: ELEMENT, B <: ELEMENT](left: STAR[A], right: STAR[B]) : QUIVER[A x B, B] =
    (left x right)(right) { _.right }

  // Helper methods for triproducts (this could obviously be extended). Note, are these used outside tests?
  def leftProjection[X <: ELEMENT, Y <: ELEMENT, Z <: ELEMENT](
    x: STAR[X], y: STAR[Y], z: STAR[Z]
  ) : QUIVER[X x Y x Z, X] =
    leftProjection(x, y) o leftProjection(x x y, z)

  def midProjection[X <: ELEMENT, Y <: ELEMENT, Z <: ELEMENT](
   x: STAR[X], y: STAR[Y], z: STAR[Z]
  ) : QUIVER[X x Y x Z, Y] =
    rightProjection(x, y) o leftProjection(x x y, z)

  def rightProjection[X <: ELEMENT, Y <: ELEMENT, Z <: ELEMENT](
   x: STAR[X], y: STAR[Y], z: STAR[Z]
  ) : QUIVER[X x Y x Z, Z] =
    rightProjection(x x y, z)
}

trait Wrappings[
  DOTINPUT[T],
  CONNECTOR[S, T]
] { topos: Topos =>
  type DOTWRAPPER[T] <: ELEMENT

  def star[T](input: DOTINPUT[T]) : STAR[DOTWRAPPER[T]]
  def quiver[S, T](connector: CONNECTOR[S, T]) : QUIVER[DOTWRAPPER[S], DOTWRAPPER[T]]

  // TODO: get rid of this abomination by introducing a map operation on DOTWRAPPER?
  def cleverQuiver[S, T](source: STAR[DOTWRAPPER[S]], target: STAR[DOTWRAPPER[T]], f: S => T): QUIVER[DOTWRAPPER[S], DOTWRAPPER[T]]
}
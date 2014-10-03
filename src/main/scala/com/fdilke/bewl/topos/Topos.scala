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

  trait Star[S <: ELEMENT] {
    val identity: QUIVER[S, S]
    def x[T <: ELEMENT](that: STAR[T]): STAR[S x T]
    def sanityTest
  }

  trait Quiver[S <: ELEMENT, T <: ELEMENT] {
    val source: STAR[S]
    val target: STAR[T]

    def o[R <: ELEMENT](that: QUIVER[R, S]) : QUIVER[R, T]
    def x[U <: ELEMENT](that: QUIVER[S, U]): QUIVER[S, T x U]
    def sanityTest
  }

  def bind[S <: ELEMENT, T <: ELEMENT](source: STAR[S], target: STAR[T], f: S => T) : QUIVER[S, T]
  // TODO rename as a proper operator: ":>()()" ?

  // TODO extras - separate into a trait?

  def leftProjection[A <: ELEMENT, B <: ELEMENT](left: STAR[A], right: STAR[B]) =
    bind[A x B, A](left x right, left, _.left)

  def rightProjection[A <: ELEMENT, B <: ELEMENT](left: STAR[A], right: STAR[B]) =
    bind[A x B, B](left x right, right, _.right)
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
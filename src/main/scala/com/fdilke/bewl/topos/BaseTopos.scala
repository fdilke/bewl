package com.fdilke.bewl.topos

trait Topos extends BaseTopos with SafeAlgebra

trait BaseTopos {
  type ELEMENT
  type STAR[S <: ELEMENT] <: Star[S]
  type QUIVER[S <: ELEMENT, T <: ELEMENT] <: Quiver[S, T]

  trait xI[T <: ELEMENT, U <: ELEMENT] { self: ELEMENT =>
    val left: T
    val right: U
  }
  type x[T <: ELEMENT, U <: ELEMENT] = xI[T, U] with ELEMENT
  object x {
    def unapply[L <: ELEMENT, R <: ELEMENT](pair: xI[L, R]): Option[(L, R)] = Some((pair.left, pair.right))
  }

  trait ~>[T <: ELEMENT, U <: ELEMENT] { self: ELEMENT =>
    def apply(t: T): U
  }
  type >[T <: ELEMENT, U <: ELEMENT] = ~>[T, U] with ELEMENT

  type UNIT <: ELEMENT
  val I : STAR[UNIT]

  type TRUTH <: ELEMENT
  val omega: STAR[TRUTH]
  val truth: QUIVER[UNIT, TRUTH]

  trait ExponentialStar[S <: ELEMENT, T <: ELEMENT] { star: STAR[S > T] =>
    val source: STAR[S]
    val target: STAR[T]
    def transpose[R <: ELEMENT](quiver: BiQuiver[R, S, T]): QUIVER[R, S > T]
    final def evaluation: BiQuiver[S > T, S, T] = biQuiver(
      this, source, target,
      { (f, s) => f(s) }
    )
  }

  trait EqualizingStar[S <: ELEMENT] { star: STAR[EqualizingElement[S] with ELEMENT] =>
    val equalizerTarget: STAR[S]
    final val inclusion: QUIVER[EqualizingElement[S] with ELEMENT, S] =
      this(equalizerTarget) { _.include }
    def restrict[R <: ELEMENT](quiver: QUIVER[R, S]): QUIVER[R, EqualizingElement[S] with ELEMENT]
  }

  trait EqualizingElement[S <: ELEMENT] { element: ELEMENT =>
    val include: S
  }

  trait Star[S <: ELEMENT] { self: STAR[S] =>
    final lazy val identity: QUIVER[S, S] = this(self)(Predef.identity)
    val toI: QUIVER[S, UNIT]
    def x[T <: ELEMENT](that: STAR[T]): STAR[S x T]
    def >[T <: ELEMENT](that: STAR[T]): ExponentialStar[S, T] with STAR[S > T]
    def sanityTest

    def apply[T <: ELEMENT](target: STAR[T])(f: S => T) : QUIVER[S, T]
  }

  trait Quiver[S <: ELEMENT, T <: ELEMENT] {
    val source: STAR[S]
    val target: STAR[T]
    val chi: QUIVER[T, TRUTH]

    def apply(s: S): T
    def ?=(that: QUIVER[S, T]): EqualizingStar[S] with STAR[EqualizingElement[S] with ELEMENT]
    def o[R <: ELEMENT](that: QUIVER[R, S]) : QUIVER[R, T]
    def x[U <: ELEMENT](that: QUIVER[S, U]): QUIVER[S, T x U]
    def \[U <: ELEMENT](monic: QUIVER[U, T]) : QUIVER[S, U]
    def sanityTest
  }

  def biQuiver[
  L <: ELEMENT,
  R <: ELEMENT,
  T <: ELEMENT
  ](
   left: STAR[L],
   right: STAR[R],
   target: STAR[T],
   f: (L, R) => T
   ) =
  BiQuiver(left, right,
    (left x right)(target) {
      case l x r =>
      f(l, r)
    })

  case class BiQuiver[
    L <: ELEMENT,
    R <: ELEMENT,
    T <: ELEMENT](
    left: STAR[L],
    right: STAR[R],
    quiver: QUIVER[L x R, T]) {
    def apply[S <: ELEMENT](
      leftQuiver: QUIVER[S, L],
      rightQuiver: QUIVER[S, R]): QUIVER[S, T] =
      quiver o (leftQuiver x rightQuiver)
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
  PRESTAR[T],
  PREQUIVER[S, T]
] { topos: BaseTopos =>
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
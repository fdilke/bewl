package com.fdilke.bewl.topos

import com.fdilke.bewl.actions.NaiveMonoidsAndActions

import scala.Function.tupled

trait Topos extends BaseTopos with NaiveMonoidsAndActions with AlgebraicMachinery with LogicalOperations

trait BaseTopos { self: LogicalOperations =>
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
      final def universally[T <: ELEMENT](target: STAR[T])(bifunc: ((L x R), T) => TRUTH) =
        BiQuiver(this, target.forAll(this)(bifunc))
    }

  type EQUALIZER[S <: ELEMENT] = EqualizingStar[S] with STAR[S]
  trait EqualizingStar[S <: ELEMENT] { star: STAR[S] =>
    val equalizerTarget: STAR[S]
    val inclusion: QUIVER[S, S]
    def restrict[R <: ELEMENT](quiver: QUIVER[R, S]): QUIVER[R, S]
  }

  trait BaseStar[S <: ELEMENT] { self: STAR[S] =>
    final lazy val identity: QUIVER[S, S] = this(self) { s => s }
    val toI: QUIVER[S, UNIT]
    def x[T <: ELEMENT](that: STAR[T]): BIPRODUCT[S, T]
    def >[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[S, T]
    def apply[T <: ELEMENT](target: STAR[T])(f: S => T) : QUIVER[S, T]
    def sanityTest
  }

  trait Star[S <: ELEMENT] extends BaseStar[S] { self: STAR[S] =>
    lazy val power = this > omega

    lazy val ∀ = (truth o toI).name.chi

    def map(f: S => S): QUIVER[S, S] = this(this)(f)
    def flatMap(f2: S => QUIVER[S, S]): BiQuiver[S, S, S] =
      (this x this).biQuiver(this) {
        case (x, y) => f2(x)(y)
      }

    lazy val ∃ =
      omega.forAll(power) {
        case (f, w) =>
          TruthObject.implies((power x omega).universally(this) {
            case ((f, w), x) => TruthObject.implies(f(x), w)
          }(f, w), w)
      }


    def forAll[R <: ELEMENT](target: STAR[R])(g: (R, S) => TRUTH): QUIVER[R, TRUTH] =
      ∀ o power.transpose(
        (target x this).biQuiver(omega)(g)
      )
  }

  trait BaseQuiver[S <: ELEMENT, T <: ELEMENT] {
    val source: STAR[S]
    val target: STAR[T]
    val chi: QUIVER[T, TRUTH]

    def apply(s: S): T
    def ?=(that: QUIVER[S, T]): EQUALIZER[S]
    def o[R <: ELEMENT](that: QUIVER[R, S]) : QUIVER[R, T]
    def \[U <: ELEMENT](monic: QUIVER[U, T]) : QUIVER[S, U]
    def sanityTest
  }

  trait Quiver[X <: ELEMENT, Y <: ELEMENT] extends BaseQuiver[X, Y] {
    def name =
      (source > target).transpose(
        (I x source).biQuiver(target) {
          case (i, x) => this(x)
        })
    def x[Z <: ELEMENT](that: QUIVER[X, Z]): QUIVER[X, Y x Z] = {
      val product = target x that.target
      source(product) {
        s => product.pair(this(s), that(s))
      }}
  }

  case class BiQuiver[
    L <: ELEMENT,
    R <: ELEMENT,
    T <: ELEMENT](
    product: BIPRODUCT[L, R],
    quiver: QUIVER[L x R, T]) {
    def apply(l: L, r: R): T = quiver(product.pair(l, r))
    def apply[S <: ELEMENT](l: QUIVER[S, L], r: QUIVER[S, R]): QUIVER[S, T] = quiver o (l x r)
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

trait Wrappings[PRESTAR[_], PREQUIVER[_, _]] { topos: BaseTopos =>
  type WRAPPER[T] <: ELEMENT

  def star[T](input: PRESTAR[T]) : STAR[WRAPPER[T]]
  def quiver[S, T](connector: PREQUIVER[S, T]) : QUIVER[WRAPPER[S], WRAPPER[T]]
  def functionAsQuiver[S, T](source: STAR[WRAPPER[S]], target: STAR[WRAPPER[T]], f: S => T): QUIVER[WRAPPER[S], WRAPPER[T]]
  def bifunctionAsBiQuiver[L, R, T] (
    left: STAR[WRAPPER[L]],
    right: STAR[WRAPPER[R]],
    target: STAR[WRAPPER[T]]
  ) (
    bifunc: (L, R) => T
  ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]]

  def bifunctionAsBiQuiver[X] (
    star: STAR[WRAPPER[X]]
  ) (
     bifunc: (X, X) => X
  ): BiQuiver[WRAPPER[X], WRAPPER[X], WRAPPER[X]] =
    bifunctionAsBiQuiver[X, X, X](star, star, star) { bifunc }
}


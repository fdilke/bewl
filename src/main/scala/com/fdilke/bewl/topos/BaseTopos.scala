package com.fdilke.bewl.topos

import com.fdilke.bewl.actions.NaiveMonoidsAndActions
import com.fdilke.bewl.helper.Memoize

import scala.Function.tupled
import scala.language.higherKinds

trait Topos extends BaseTopos with NaiveMonoidsAndActions with AlgebraicMachinery with LogicalOperations

trait BaseTopos { self: LogicalOperations =>
  type ~
  type DOT[S <: ~] <: Star[S]
  type QUIVER[S <: ~, T <: ~] <: Quiver[S, T]

  type >[T <: ~, U <: ~] <: (T => U) with ~
  type x[T <: ~, U <: ~] <: (T, U) with ~

  type UNIT <: ~
  val I : DOT[UNIT]

  type TRUTH <: ~
  val omega: DOT[TRUTH]
  val truth: QUIVER[UNIT, TRUTH]

  implicit class OmegaEnrichments(truthValue: TRUTH) {
    def >(that: TRUTH) = TruthObject.implies(truthValue, that)
    def ^(that: TRUTH) = TruthObject.and(truthValue, that)
    def v(that: TRUTH) = TruthObject.or(truthValue, that)
  }

  type EXPONENTIAL[S <: ~, T <: ~] = ExponentialStar[S, T, S > T] with DOT[S > T]
  trait ExponentialStar[S <: ~, T <: ~, S_T <: (S => T) with ~] { star: DOT[S_T] =>
    val source: DOT[S]
    val target: DOT[T]

    def transpose[R <: ~](biQuiver: BiQuiver[R, S, T]): QUIVER[R, S_T]
    final def evaluation: BiQuiver[S_T, S, T] =
      (this x source).biQuiver(target) { (f, s) => f(s) }
  }

  type BIPRODUCT[L <: ~, R <: ~] = BiproductStar[L, R, L x R] with DOT[L x R]

  trait BiproductStar[L <: ~, R <: ~, LxR <: (L, R) with ~] { star: DOT[LxR] =>
    val left: DOT[L]
    val right: DOT[R]
    def pair(l: L, r: R): LxR
    final lazy val π0 = star(left) { _._1 }
    final lazy val π1 = star(right) { _._2 }

    final private val hackedThis: BIPRODUCT[L, R] = this.asInstanceOf[BIPRODUCT[L, R]]

    final def biQuiver[T <: ~](
      target: DOT[T]
      ) (
      bifunc: (L, R) => T
      ) : BiQuiver[L, R, T] =
      BiQuiver(hackedThis, hackedThis(target) (
        tupled[L,R,T](bifunc)
      ))
      final def universally[T <: ~](target: DOT[T])(bifunc: ((L x R), T) => TRUTH) =
        BiQuiver(hackedThis, hackedThis.forAll(target)(bifunc))
      final def existentially[T <: ~](target: DOT[T])(bifunc: ((L x R), T) => TRUTH) =
        BiQuiver(hackedThis, hackedThis.exists(target)(bifunc))
    }

  type EQUALIZER[S <: ~] = EqualizingStar[S] with DOT[S]
  trait EqualizingStar[S <: ~] { star: DOT[S] =>
    val equalizerTarget: DOT[S]
    val inclusion: QUIVER[S, S]
    def restrict[R <: ~](quiver: QUIVER[R, S]): QUIVER[R, S]
  }

  trait BaseStar[S <: ~] { self: DOT[S] =>
    val toI: QUIVER[S, UNIT]
    val globals: Traversable[QUIVER[UNIT, S]]
    def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T]
    def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T]
    def apply[T <: ~](target: DOT[T])(f: S => T) : QUIVER[S, T]
    def sanityTest
  }

  trait Star[S <: ~] extends BaseStar[S] { star: DOT[S] =>

    final lazy val identity: QUIVER[S, S] = this(star) { s => s }
    final private val memoizedProduct =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[U <: ~] = BIPRODUCT[S, U]})#λ,
        ~
      ] (xUncached)

    final def x[U <: ~](that: DOT[U]): BIPRODUCT[S, U] = memoizedProduct(that)

    final private val memoizedExponential =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[T <: ~] = EXPONENTIAL[S, T]})#λ,
        ~
      ] (`>Uncached`)

    final def >[T <: ~](that: DOT[T]): EXPONENTIAL[S, T] = memoizedExponential(that)

    final lazy val toTrue = truth o toI
    final lazy val power = this > omega
    final lazy val ∀ = toTrue.name.chi
    final lazy val squared = this x this

    final def map(f: S => S) = this(this)(f)
    final def flatMap(f2: S => QUIVER[S, S]) =
      (this x this).biQuiver(this) { f2(_)(_) }

    final lazy val ∃ =
      power.forAll(omega) { (f, w) =>
          (power x omega).universally(this) {
            case ((f, w), x) => f(x) > w
          }(f, w) > w
      }

    final def preForAll[R <: ~](source: DOT[R])(g: (R, S) => TRUTH): QUIVER[R, TRUTH] =
      ∀ o power.transpose(
        (source x this).biQuiver(omega)(g)
      )

    final def forAll[T <: ~](target: DOT[T])(g: (S, T) => TRUTH): QUIVER[S, TRUTH] =
      target.preForAll(this)(g)

    final def forAll[
      T <: ~, 
      U <: ~
    ] (
      target: DOT[T], 
      target2: DOT[U]
    ) (
      g: (S, T, U) => TRUTH
    ): QUIVER[S, TRUTH] =
      forAll(target) { (x, t) =>
        target.forAll(target2) { (t, u) => 
            g(x, t, u)
          }(t)
      }

    // TODO refactor to be properly variadic
    final def forAll[
      T <: ~, 
      U <: ~,
      V <: ~
    ] (
      target: DOT[T], 
      target2: DOT[U],
      target3: DOT[V]
    ) (
      g: (S, T, U, V) => TRUTH
    ): QUIVER[S, TRUTH] =
      forAll(target) { (x, t) =>
        target.forAll(target2) { (t, u) => 
          target2.forAll(target3) { (u, v) =>
              g(x, t, u, v)
            }(u)
          }(t)
        }

    final def preExists[R <: ~](source: DOT[R])(g: (R, S) => TRUTH): QUIVER[R, TRUTH] =
      ∃ o power.transpose(
        (source x this).biQuiver(omega)(g)
      )

    final def exists[T <: ~](target: DOT[T])(g: (S, T) => TRUTH): QUIVER[S, TRUTH] =
      target.preExists(this)(g)

    final lazy val diagonal: QUIVER[S, S x S] =
      this(squared) { x => squared.pair(x, x) }

    final lazy val =?= : BiQuiver[S, S, TRUTH] =
      BiQuiver(squared, diagonal.chi)

    final lazy val singleton: QUIVER[S, S > TRUTH] =
      power transpose =?=

    final def >>[T <: ~](
      target: DOT[T]
    ): Traversable[
      QUIVER[S, T]
    ] =
      (this > target).globals map { global =>
        star(target) { s =>
          global(star.toI(s))(s)
        }}
  }

  trait BaseQuiver[S <: ~, T <: ~] {
    val source: DOT[S]
    val target: DOT[T]
    val chi: QUIVER[T, TRUTH]

    def apply(s: S): T
    def ?=(that: QUIVER[S, T]): EQUALIZER[S]
    def o[R <: ~](that: QUIVER[R, S]) : QUIVER[R, T]
    def \[U <: ~](monic: QUIVER[U, T]) : QUIVER[S, U]
    def sanityTest
  }

  trait Quiver[S <: ~, T <: ~] extends BaseQuiver[S, T] { self: QUIVER[S, T] =>
    final lazy val name =
      (source > target).transpose(
        (I x source).biQuiver(target) {
          (i, x) => this(x)
        })

    final def x[U <: ~](that: QUIVER[S, U]): QUIVER[S, T x U] = {
      val product = target x that.target
      source(product) {
        s => product.pair(this(s), that(s))
      }}

    final def toBool(implicit eq: =:=[T, TRUTH]): Boolean =
      this == source.toTrue

    final def whereTrue(implicit eq: =:=[T, TRUTH]): EQUALIZER[S] =
      this.asInstanceOf[QUIVER[S, TRUTH]] ?= source.toTrue

    final lazy val isMonic: Boolean =
      source.forAll(source) {
        (s, t) => target.=?=(
          this(s), this(t)
        ) > source.=?=(s, t)
      } toBool

    final lazy val isEpic: Boolean =
      target.exists(source) {
        (t, s) => target.=?=(
          t, this(s)
        )
      } toBool

    final lazy val isIso: Boolean =
      isMonic && isEpic

    final lazy val inverse: QUIVER[T, S] = 
      source.power.transpose(
        (target x source).biQuiver(omega) {
          (t, s) => target.=?=(t, this(s))  
        }
      ) \ source.singleton
  }

  case class BiQuiver[
    L <: ~,
    R <: ~,
    T <: ~
  ] (
    product: BIPRODUCT[L, R],
    quiver: QUIVER[L x R, T]) {
    def apply(l: L, r: R): T = quiver(product.pair(l, r))
    def apply[S <: ~](l: QUIVER[S, L], r: QUIVER[S, R]): QUIVER[S, T] = quiver o (l x r)
  }

  // Helper methods for triproducts (this could obviously be extended).
  def leftProjection[X <: ~, Y <: ~, Z <: ~](
    x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : QUIVER[X x Y x Z, X] =
    (x x y).π0 o (x x y x z).π0

  def midProjection[X <: ~, Y <: ~, Z <: ~](
   x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : QUIVER[X x Y x Z, Y] =
    (x x y).π1 o (x x y x z).π0

  def rightProjection[X <: ~, Y <: ~, Z <: ~](
   x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : QUIVER[X x Y x Z, Z] =
    (x x y x z).π1
}

trait Wrappings[BASE, PREDOT[_ <: BASE], PREQUIVER[_ <: BASE, _ <: BASE]] { topos: BaseTopos =>
  type WRAPPER[T <: BASE] <: ~

  def star[T <: BASE](input: PREDOT[T]) : DOT[WRAPPER[T]]
  def quiver[S <: BASE, T <: BASE](connector: PREQUIVER[S, T]) : QUIVER[WRAPPER[S], WRAPPER[T]]
  def functionAsQuiver[S <: BASE, T <: BASE](source: DOT[WRAPPER[S]], target: DOT[WRAPPER[T]], f: S => T): QUIVER[WRAPPER[S], WRAPPER[T]]
  def bifunctionAsBiQuiver[L <: BASE, R <: BASE, T <: BASE] (
    left: DOT[WRAPPER[L]],
    right: DOT[WRAPPER[R]],
    target: DOT[WRAPPER[T]]
  ) (
    bifunc: (L, R) => T
  ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]]

  def bifunctionAsBiQuiver[X <: BASE] (
    star: DOT[WRAPPER[X]]
  ) (
     bifunc: (X, X) => X
  ): BiQuiver[WRAPPER[X], WRAPPER[X], WRAPPER[X]] =
    bifunctionAsBiQuiver[X, X, X](star, star, star) { bifunc }
}



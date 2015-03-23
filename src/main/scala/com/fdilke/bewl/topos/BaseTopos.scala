package com.fdilke.bewl.topos

import com.fdilke.bewl.actions.NaiveMonoidsAndActions
import com.fdilke.bewl.helper.Memoize

import scala.Function.tupled
import scala.language.higherKinds

trait Topos extends BaseTopos with NaiveMonoidsAndActions with AlgebraicMachinery with LogicalOperations

trait BaseTopos { self: LogicalOperations =>
  type ~
  type DOT[S <: ~] <: Dot[S]
  type ARROW[S <: ~, T <: ~] <: Arrow[S, T]

  type >[T <: ~, U <: ~] <: (T => U) with ~
  type x[T <: ~, U <: ~] <: (T, U) with ~

  type UNIT <: ~
  val I : DOT[UNIT]

  type TRUTH <: ~
  val omega: DOT[TRUTH]
  val truth: ARROW[UNIT, TRUTH]

  implicit class OmegaEnrichments(truthValue: TRUTH) {
    def >(that: TRUTH) = TruthObject.implies(truthValue, that)
    def ^(that: TRUTH) = TruthObject.and(truthValue, that)
    def v(that: TRUTH) = TruthObject.or(truthValue, that)
  }

  type EXPONENTIAL[S <: ~, T <: ~] = ExponentialDot[S, T, S > T] with DOT[S > T]
  trait ExponentialDot[S <: ~, T <: ~, S_T <: (S => T) with ~] { dot: DOT[S_T] =>
    val source: DOT[S]
    val target: DOT[T]

    def transpose[R <: ~](biArrow: BiArrow[R, S, T]): ARROW[R, S_T]
    final def evaluation: BiArrow[S_T, S, T] =
      (this x source).biArrow(target) { (f, s) => f(s) }
  }

  type BIPRODUCT[L <: ~, R <: ~] = BiproductDot[L, R, L x R] with DOT[L x R]

  trait BiproductDot[L <: ~, R <: ~, LxR <: (L, R) with ~] { dot: DOT[LxR] =>
    val left: DOT[L]
    val right: DOT[R]
    def pair(l: L, r: R): LxR
    final lazy val π0 = dot(left) { _._1 }
    final lazy val π1 = dot(right) { _._2 }

    final private val hackedThis: BIPRODUCT[L, R] = this.asInstanceOf[BIPRODUCT[L, R]]

    final def biArrow[T <: ~](
      target: DOT[T]
      ) (
      bifunc: (L, R) => T
      ) : BiArrow[L, R, T] =
      BiArrow(hackedThis, hackedThis(target) (
        tupled[L,R,T](bifunc)
      ))
      final def universally[T <: ~](target: DOT[T])(bifunc: ((L x R), T) => TRUTH) =
        BiArrow(hackedThis, hackedThis.forAll(target)(bifunc))
      final def existentially[T <: ~](target: DOT[T])(bifunc: ((L x R), T) => TRUTH) =
        BiArrow(hackedThis, hackedThis.exists(target)(bifunc))
    }

  type EQUALIZER[S <: ~] = EqualizingDot[S] with DOT[S]
  trait EqualizingDot[S <: ~] { dot: DOT[S] =>
    val equalizerTarget: DOT[S]
    val inclusion: ARROW[S, S]
    def restrict[R <: ~](arrow: ARROW[R, S]): ARROW[R, S]
  }

  trait BaseDot[S <: ~] { self: DOT[S] =>
    val toI: ARROW[S, UNIT]
    val globals: Traversable[ARROW[UNIT, S]]
    def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T]
    def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T]
    def apply[T <: ~](target: DOT[T])(f: S => T) : ARROW[S, T]
    def sanityTest
  }

  trait Dot[S <: ~] extends BaseDot[S] { dot: DOT[S] =>

    final lazy val identity: ARROW[S, S] = this(dot) { s => s }
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
    final def flatMap(f2: S => ARROW[S, S]) =
      (this x this).biArrow(this) { f2(_)(_) }

    final lazy val ∃ =
      power.forAll(omega) { (f, w) =>
          (power x omega).universally(this) {
            case ((f, w), x) => f(x) > w
          }(f, w) > w
      }

    final def preForAll[R <: ~](source: DOT[R])(g: (R, S) => TRUTH): ARROW[R, TRUTH] =
      ∀ o power.transpose(
        (source x this).biArrow(omega)(g)
      )

    final def forAll[T <: ~](target: DOT[T])(g: (S, T) => TRUTH): ARROW[S, TRUTH] =
      target.preForAll(this)(g)

    final def forAll[
      T <: ~, 
      U <: ~
    ] (
      target: DOT[T], 
      target2: DOT[U]
    ) (
      g: (S, T, U) => TRUTH
    ): ARROW[S, TRUTH] =
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
    ): ARROW[S, TRUTH] =
      forAll(target) { (x, t) =>
        target.forAll(target2) { (t, u) => 
          target2.forAll(target3) { (u, v) =>
              g(x, t, u, v)
            }(u)
          }(t)
        }

    final def preExists[R <: ~](source: DOT[R])(g: (R, S) => TRUTH): ARROW[R, TRUTH] =
      ∃ o power.transpose(
        (source x this).biArrow(omega)(g)
      )

    final def exists[T <: ~](target: DOT[T])(g: (S, T) => TRUTH): ARROW[S, TRUTH] =
      target.preExists(this)(g)

    final lazy val diagonal: ARROW[S, S x S] =
      this(squared) { x => squared.pair(x, x) }

    final lazy val =?= : BiArrow[S, S, TRUTH] =
      BiArrow(squared, diagonal.chi)

    final lazy val singleton: ARROW[S, S > TRUTH] =
      power transpose =?=

    final def >>[T <: ~](
      target: DOT[T]
    ): Traversable[
      ARROW[S, T]
    ] =
      (this > target).globals map { global =>
        dot(target) { s =>
          global(dot.toI(s))(s)
        }}
  }

  trait BaseArrow[S <: ~, T <: ~] {
    val source: DOT[S]
    val target: DOT[T]
    val chi: ARROW[T, TRUTH]

    def apply(s: S): T
    def ?=(that: ARROW[S, T]): EQUALIZER[S]
    def o[R <: ~](that: ARROW[R, S]) : ARROW[R, T]
    def \[U <: ~](monic: ARROW[U, T]) : ARROW[S, U]
    def sanityTest
  }

  trait Arrow[S <: ~, T <: ~] extends BaseArrow[S, T] { self: ARROW[S, T] =>
    final lazy val name =
      (source > target).transpose(
        (I x source).biArrow(target) {
          (i, x) => this(x)
        })

    final def x[U <: ~](that: ARROW[S, U]): ARROW[S, T x U] = {
      val product = target x that.target
      source(product) {
        s => product.pair(this(s), that(s))
      }}

    final def toBool(implicit eq: =:=[T, TRUTH]): Boolean =
      this == source.toTrue

    final def whereTrue(implicit eq: =:=[T, TRUTH]): EQUALIZER[S] =
      this.asInstanceOf[ARROW[S, TRUTH]] ?= source.toTrue

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

    final lazy val inverse: ARROW[T, S] = 
      source.power.transpose(
        (target x source).biArrow(omega) {
          (t, s) => target.=?=(t, this(s))  
        }
      ) \ source.singleton
  }

  case class BiArrow[
    L <: ~,
    R <: ~,
    T <: ~
  ] (
    product: BIPRODUCT[L, R],
    arrow: ARROW[L x R, T]) {
    def apply(l: L, r: R): T = arrow(product.pair(l, r))
    def apply[S <: ~](l: ARROW[S, L], r: ARROW[S, R]): ARROW[S, T] = arrow o (l x r)
  }

  // Helper methods for triproducts (this could obviously be extended).
  def leftProjection[X <: ~, Y <: ~, Z <: ~](
    x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : ARROW[X x Y x Z, X] =
    (x x y).π0 o (x x y x z).π0

  def midProjection[X <: ~, Y <: ~, Z <: ~](
   x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : ARROW[X x Y x Z, Y] =
    (x x y).π1 o (x x y x z).π0

  def rightProjection[X <: ~, Y <: ~, Z <: ~](
   x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : ARROW[X x Y x Z, Z] =
    (x x y x z).π1
}

trait Wrappings[BASE, PREDOT[_ <: BASE], PREARROW[_ <: BASE, _ <: BASE]] { topos: BaseTopos =>
  type WRAPPER[T <: BASE] <: ~

  def makeDot[T <: BASE](input: PREDOT[T]) : DOT[WRAPPER[T]]
  def makeArrow[S <: BASE, T <: BASE](connector: PREARROW[S, T]) : ARROW[WRAPPER[S], WRAPPER[T]]
  def functionAsArrow[S <: BASE, T <: BASE](
    source: DOT[WRAPPER[S]], 
    target: DOT[WRAPPER[T]], 
    f: S => T
  ): ARROW[WRAPPER[S], WRAPPER[T]]
  def bifunctionAsBiArrow[L <: BASE, R <: BASE, T <: BASE] (
    left: DOT[WRAPPER[L]],
    right: DOT[WRAPPER[R]],
    target: DOT[WRAPPER[T]]
  ) (
    bifunc: (L, R) => T
  ): BiArrow[WRAPPER[L], WRAPPER[R], WRAPPER[T]]

  def bifunctionAsBiArrow[X <: BASE] (
    dot: DOT[WRAPPER[X]]
  ) (
     bifunc: (X, X) => X
  ): BiArrow[WRAPPER[X], WRAPPER[X], WRAPPER[X]] =
    bifunctionAsBiArrow[X, X, X](dot, dot, dot) { bifunc }
}



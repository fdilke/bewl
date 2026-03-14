package com.fdilke.bewl.topos

import com.fdilke.bewl.helper.{⊕, Memoize, VerifyLength}

import scala.language.postfixOps

trait BaseTopos {

  Ɛ: ToposPrerequisites =>

  val name: String

  type ~
  type DOT[S <: ~] <: Dot[S]
  type >[S <: ~, T <: ~] <: Arrow[S, T]

  type →[T <: ~, U <: ~] <: ~
  type x[T <: ~, U <: ~] = (T ⊕ U) with ~

  type UNIT <: ~
  val I: DOT[UNIT]

  type TRUTH <: ~
  val omega: DOT[TRUTH]
  val truth: UNIT > TRUTH
  val optionalGenerator: Option[DOT[_ <: ~]]

  type EXPONENTIAL[S <: ~, T <: ~] =
    ExponentialDot[S, T] with DOT[S → T]

  trait ExponentialDot[
    S <: ~,
    T <: ~
  ] { dot: DOT[S → T] =>
    val source: DOT[S]
    val target: DOT[T]

    def transpose[R <: ~](
      biArrow: BiArrow[R, S, T]
    ): R > (S → T)

    final def transpose[R <: ~](
      index: DOT[R]
    )(
      bifunc: (R, S) => T
    ): R > (S → T) =
      transpose(
        index.x(source).biArrow(target)(bifunc)
      )

    def evaluate(
      function: S → T,
      arg: S
    ): T

    final def evaluation: BiArrow[S → T, S, T] =
      this.x(source).biArrow(target) {
        evaluate(_, _)
      }
  }

  type BIPRODUCT[
    L <: ~,
    R <: ~
  ] = BiproductDot[
    L,
    R
  ] with DOT[
      L x R
    ]

  trait BiproductDot[
    L <: ~,
    R <: ~
  ] { product: DOT[L x R] =>
    val left: DOT[L]
    val right: DOT[R]
    def pair(l: L, r: R): L x R

    final lazy val π0 =
      product(left)(_._1)

    final lazy val π1 =
      product(right)(_._2)

    final def biArrow[T <: ~](
      target: DOT[T]
    )(
      bifunc: (L, R) => T
    ): BiArrow[L, R, T] =
      BiArrow(this, this(target) {
        case x ⊕ y => bifunc(x, y)
      })

    final def relation(
      bifunc: (L, R) => TRUTH
    ): Relation[L, R] =
      Relation(
        left,
        right,
        biArrow(omega)(bifunc)
      )

    final def relation(
      criterion: L x R > TRUTH
    ): Relation[L, R] =
      Relation(
        left,
        right,
        BiArrow(
          product,
          criterion
        )
      )

    final def universally[T <: ~](
      target: DOT[T]
    )(
      bifunc: (L x R, T) => TRUTH
    ) =
      BiArrow(
        this,
        this.forAll(target)(bifunc)
      )

    final def existsMid[T <: ~](
      mid: DOT[T]
    )(
      trifunc: (L, T, R) => TRUTH
    ): BiArrow[L, R, TRUTH] =
      BiArrow[L, R, TRUTH](
        product,
        exists(
          mid
        ) { (lr, t) =>
          val l = π0(lr)
          val r = π1(lr)
          trifunc(l, t, r)
        }
      )

    final def existsMidWithImages[T <: ~](
      mid: DOT[T]
    )(
      trifunc: (L, T, R) => TRUTH
    ): BiArrow[L, R, TRUTH] =
      BiArrow[L, R, TRUTH](
        product,
        existsViaImage(
          mid
        ) { (lr, t) =>
          val l = π0(lr)
          val r = π1(lr)
          trifunc(l, t, r)
        }
      )
  }

  type EQUALIZER[S <: ~] =
    EqualizingDot[S] with DOT[S]

  trait EqualizingDot[S <: ~] { dot: DOT[S] =>
    val equalizerTarget: DOT[S]
    def restrict[R <: ~](
      arrow: R > S
    ): R > S

    lazy val inclusion: S > S =
      dot(equalizerTarget)(s => s)
  }

  private object InitialDot {
    lazy val O = falsity whereTrue
    def fromO[X <: ~](dot: DOT[X]) = {
      val xO = dot.toTrue ?= (falsity.o(dot.toI))
      val xOtoO = O.restrict(xO.toI)
      xO.inclusion.o(xOtoO.inverse)
    }
  }
  type VOID = UNIT // TODO: fix with strict equalizers
  type QUOTIENT[X <: ~] = X → TRUTH
  type COEQUALIZER[X <: ~] = X → TRUTH

  lazy val O: DOT[VOID] = InitialDot.O

  implicit class RichLeftElement[
    A <: ~
  ](
    a: A
  ) {
    def ⊕⊕[B <: ~](
      b: B
    )(
      implicit biproduct: BIPRODUCT[A, B]
    ): A x B =
      biproduct.pair(a, b)
  }

  implicit class RichFunctionalElement[
    S <: ~,
    T <: ~
  ](
    f: S → T
  )(
    implicit exp: EXPONENTIAL[S, T]
  ) {
    def apply(
      s: S
    ): T =
      exp.evaluate(f, s)
  }

  trait BaseDot[S <: ~] { self: DOT[S] =>
    val toI: S > UNIT
    val globals: Iterable[UNIT > S]
    def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T]
    def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T]
    def apply[T <: ~](target: DOT[T])(f: S => T): S > T
    def sanityTest: Unit
  }

  trait Dot[S <: ~] extends BaseDot[S] {
    dot: DOT[S] =>

    final lazy val identity: S > S =
      dot(dot)(s => s)

    final private val memoizedProduct =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[U <: ~] = BIPRODUCT[S, U] })#λ,
        ~
      ](xUncached)

    final def x[U <: ~](
      that: DOT[U]
    ): BIPRODUCT[S, U] =
      memoizedProduct(that)

    final private val memoizedExponential =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[T <: ~] = EXPONENTIAL[S, T] })#λ,
        ~
      ](`>Uncached`)

    final def >[T <: ~](
      that: DOT[T]
    ): EXPONENTIAL[S, T] =
      memoizedExponential(that)

    final private val memoizedCoproduct =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[T <: ~] = Coproduct[S, T] })#λ,
        ~
      ](`+Uncached`)

    final def ⊔[T <: ~](
      that: DOT[T]
    ): Coproduct[S, T] =
      memoizedCoproduct(that)

    final lazy val toTrue = truth.o(toI)
    final lazy val power = this > omega
    final lazy val ∀ = toTrue.name.chi
    final lazy val squared = dot.x(dot)

    final def map(f: S => S) =
      dot(dot)(f)

    final def flatMap(
      bifunc: S => S > S
    ) =
      this.x(this).biArrow(this) {
        bifunc(_)(_)
      }

    final lazy val ∃ : S → TRUTH > TRUTH =
      power.forAll(omega) { (f, w) =>
        implicit val anonImplicit: EXPONENTIAL[S, TRUTH] = power
        power
          .x(omega)
          .universally(dot) {
            case (f ⊕ w, x) => {
              f(x) → w
            }
          }(f, w) → w
      }

    final def preForAll[R <: ~](
      source: DOT[R]
    )(
      bifunc: (R, S) => TRUTH
    ): R > TRUTH =
      ∀.o(power.transpose(source)(bifunc))

    final def forAll[T <: ~](
      target: DOT[T]
    )(
      g: (S, T) => TRUTH
    ): S > TRUTH =
      target.preForAll(dot)(g)

    final def forAll[
      T <: ~,
      U <: ~
    ](
      target: DOT[T],
      target2: DOT[U]
    )(
      g: (S, T, U) => TRUTH
    ): S > TRUTH =
      forAll(target)((x, t) => target.forAll(target2)((t, u) => g(x, t, u))(t))

    // TODO refactor to be properly variadic
    final def forAll[
      T <: ~,
      U <: ~,
      V <: ~
    ](
      target: DOT[T],
      target2: DOT[U],
      target3: DOT[V]
    )(
      g: (S, T, U, V) => TRUTH
    ): S > TRUTH =
      forAll(target) { (x, t) =>
        target.forAll(target2)((t, u) => target2.forAll(target3)((u, v) => g(x, t, u, v))(u))(
          t
        )
      }

    final def whereAll[T <: ~](
      target: DOT[T]
    )(
      g: (S, T) => TRUTH
    ): EQUALIZER[S] =
      forAll(target)(g).whereTrue

    final def whereAll[T <: ~, U <: ~](
      target: DOT[T],
      target2: DOT[U]
    )(
      g: (S, T, U) => TRUTH
    ): EQUALIZER[S] =
      forAll(target, target2)(g).whereTrue

    final def whereAll[T <: ~, U <: ~, V <: ~](
      target: DOT[T],
      target2: DOT[U],
      target3: DOT[V]
    )(
      g: (S, T, U, V) => TRUTH
    ): EQUALIZER[S] =
      forAll(target, target2, target3)(g).whereTrue

    final private def preExists[R <: ~](
      source: DOT[R]
    )(
      bifunc: (R, S) => TRUTH
    ): R > TRUTH =
      ∃.o(power.transpose(source)(bifunc))

    final def existsViaE[T <: ~](
      target: DOT[T]
    )(
      g: (S, T) => TRUTH
    ): S > TRUTH =
      target.preExists(this)(g)

    final def exists[T <: ~](
      target: DOT[T]
    )(
      g: (S, T) => TRUTH
    ): S > TRUTH =
      existsViaE(target)(g)

    final def existsViaImage[T <: ~](
      target: DOT[T]
    )(
      g: (S, T) => TRUTH
    ): S > TRUTH = {
      val dotTarget =
        dot.x(target)

      val criterion: S x T > TRUTH =
        dotTarget.biArrow(omega) {
          g
        } arrow

      val hh: EQUALIZER[S x T] =
        criterion.whereTrue

      val kk: EQUALIZER[S] =
        dotTarget.π0.o(hh.inclusion).image

      kk.inclusion.chi
    }

    final lazy val diagonal: S > (S x S) =
      this(squared) { x =>
        implicit val anonImplicit: BIPRODUCT[S, S] = squared
        x ⊕⊕ x
      }

    final lazy val =?= : BiArrow[S, S, TRUTH] =
      BiArrow(squared, diagonal.chi)

    final lazy val singleton: S > (S → TRUTH) =
      power.transpose(=?=)

    def >>[T <: ~](
      target: DOT[T]
    ): Iterable[
      S > T
    ] =
      (dot > target).globals.map { global =>
        dot(target) { s =>
          implicit val anonImplicit = dot > target
          global(dot.toI(s))(s)
        }
      }

    final lazy val fromO: VOID > S =
      InitialDot.fromO(dot)

    final lazy val pac =
      new PartialArrowClassifier(dot)

    final def *-[T <: ~](
      that: DOT[T]
    ) =
      this.x(that).π0

    final def -*[T <: ~](
      that: DOT[T]
    ) =
      this.x(that).π1

    final def `+Uncached`[T <: ~](
      that: DOT[T]
    ) =
      new Coproduct(this, that)

    final def +-[T <: ~](
      that: DOT[T]
    ) =
      (this ⊔ that).injectLeft

    final def -+[T <: ~](
      that: DOT[T]
    ) =
      (this ⊔ that).injectRight

    def +[U <: ~](
      dash: DOT[U]
    ) =
      (dot ⊔ dash).coproduct

    def arrowFromFunctionalRelation[B <: ~](
      target: DOT[B]
    )(
      predicate: (S, B) => TRUTH
    ): S > B = {
      val product = dot.x(target)
      val graph = product.where {
        ⊕.tupled(predicate)
      } inclusion

      (product.π1.o(graph)) / (product.π0.o(graph))
    }

    // Contravariant exponential functor
    final def >[
      T <: ~,
      U <: ~
    ](
      arrow: T > U
    ): (U → S) > (T → S) =
      (arrow.source > dot).transpose(
        arrow.target > dot
      ) { (s_u, t) =>
        implicit val anonImplicit = arrow.target > dot
        s_u(arrow(t))
      }

    final def ⋀(
      family: (S → TRUTH) > TRUTH
    ): S > TRUTH =
      forAll(family.whereTrue) { (s, g) =>
        implicit val anonImplicit = power
        g(s)
      }

    final def where(
      condition: S => TRUTH
    ): EQUALIZER[S] =
      dot(omega) {
        condition
      } ?= dot.toTrue

    final def universally(
      criterion: S => TRUTH
    ): Boolean =
      dot(omega) {
        criterion
      }.toBool

    final def /(
      equiv: Relation[S, S]
    ): Quotient[S] =
      new Quotient(dot, equiv)

    final def /(
      equiv: S x S > TRUTH
    ): Quotient[S] =
      dot /
        squared.relation(
          equiv
        )

    final def /(
      equiv: (S, S) => TRUTH
    ): Quotient[S] =
      dot /
        squared.relation(
          equiv
        )

    final lazy val isInjective: Boolean =
      singleton isSection

    final lazy val isMinimal: Boolean =
      VerifyLength(>>(omega).toSeq, 2)

    final def congruences: Iterable[Relation[S, S]] =
      (squared >> omega)
        .map { arrow =>
          Relation(
            dot,
            dot,
            BiArrow(
              squared,
              arrow
            )
          )
        }
        .filter {
          _.isEquivalence
        }

    final lazy val isSimple: Boolean =
      VerifyLength(congruences toSeq, 2)

    def size(): Int =
      optionalGenerator
        .map {
          _ >> dot size
        }
        .getOrElse {
          throw new IllegalArgumentException(
            "Cannot size dots in this topos - no generator available"
          )
        }
  }

  trait BaseArrow[S <: ~, T <: ~] {
    val source: DOT[S]
    val target: DOT[T]
    val chi: T > TRUTH

    def apply(s: S): T
    def ?=(that: S > T): EQUALIZER[S]
    def o[R <: ~](that: R > S): R > T
    def \[U <: ~](monic: U > T): S > U
    def sanityTest: Unit
  }

  trait Arrow[S <: ~, T <: ~] extends BaseArrow[S, T] {
    arrow: S > T =>

    final lazy val name: UNIT > (S → T) =
      (source > target).transpose(I)((i, x) => arrow(x))

    final def x[U <: ~](
      that: S > U
    ): S > (T x U) = {
      implicit val product: BIPRODUCT[T, U] =
        target.x(that.target)

      source(product)(s => this(s) ⊕⊕ that(s))
    }

    final def toBool(
      implicit eq: T =:= TRUTH
    ): Boolean =
      arrow == source.toTrue

    final def whereTrue(
      implicit eq: T =:= TRUTH
    ): EQUALIZER[S] =
      arrow.asInstanceOf[S > TRUTH] ?=
        source.toTrue

    final lazy val isMonic: Boolean =
      source
        .forAll(source) { (s, t) =>
          target.=?=(
            this(s),
            this(t)
          ) → source.=?=(
            s,
            t
          )
      } toBool

    final lazy val isEpic: Boolean =
      target
        .exists(source) { (t, s) =>
          target.=?=(
            t,
            arrow(s)
          )
      } toBool

    final lazy val isIso: Boolean =
      isMonic && isEpic

    final lazy val isSection: Boolean =
      (target >> source).exists { left: T > S => (left.o(arrow)) == source.identity }

    final lazy val isRetraction: Boolean =
      (target >> source).exists { right: T > S => (arrow.o(right)) == target.identity }

    final lazy val inverse: T > S =
      source.power.transpose(target) { (t, s) =>
        target.=?=(t, arrow(s))
      } \ source.singleton

    def /[R <: ~](iso: S > R): R > T =
      arrow.o(iso.inverse)

    def +[U <: ~](that: U > T) =
      (source ⊔ that.source).sum(
        this,
        that
      )

    lazy val image: EQUALIZER[T] =
      imageFinder.image(
        arrow
      )

    def factorizeEpiMono: (
      S > T,
      T > T
    ) = (
      image.restrict(arrow),
      image.inclusion
    )

    final def =?( // slow ("pure") coequalizer
      that: S > T
    ): Quotient[T] = {
      implicit val t2 = target.squared
      implicit val t2Power = t2.power

      val congruences: EQUALIZER[T x T → TRUTH] =
        t2Power
          .whereAll(target)((ssp, t) => ssp(t ⊕⊕ t))
          .whereAll(target, target) { (ssp, t, u) =>
            ssp(t ⊕⊕ u) →
              ssp(u ⊕⊕ t)
          }
          .whereAll(target, target, target) { (ssp, t, u, v) =>
            ssp(t ⊕⊕ u) ∧
              ssp(u ⊕⊕ v) →
              ssp(t ⊕⊕ v)
          }
          .whereAll(source) { (ssp, s) =>
            ssp(
              this(s) ⊕⊕ that(s)
            )
          }

      target / { (t, u) =>
        t2.⋀(
          congruences.inclusion.chi
        )(
          t ⊕⊕ u
        )
      }
    }

    final def =?!( // fast ("hybrid") coequalizer
      that: S > T
    ): Quotient[T] =
      target /
        target.squared
          .relation(
            arrow.x(that).image.inclusion chi
          )
          .toEquivalence

    // Contravariant exponential functor
    final def >[
      U <: ~
    ](
      exponent: DOT[U]
    ): (U → S) > (U → T) =
      (exponent > target).transpose(
        exponent > source
      ) { (s_u, u) =>
        implicit val anonImplicit = exponent > source
        arrow(s_u(u))
      }
  }

  case class BiArrow[
    L <: ~,
    R <: ~,
    T <: ~
  ](
    product: BIPRODUCT[L, R],
    arrow: L x R > T
  ) extends (
      (
        L,
        R
      ) => T
    ) {
    def apply(
      l: L,
      r: R
    ): T =
      arrow {
        implicit val anonImplicit: BIPRODUCT[L, R] = product
        l ⊕⊕ r
      }

    def apply[
      S <: ~
    ](
      l: S > L,
      r: S > R
    ): S > T =
      arrow.o(l.x(r))

    def sanityTest =
      arrow.sanityTest
  }

  // Helper methods for triple products (this could obviously be extended).
  def leftProjection[X <: ~, Y <: ~, Z <: ~](
    x: DOT[X],
    y: DOT[Y],
    z: DOT[Z]
  ): X x Y x Z > X =
    x.x(y).π0.o(x.x(y).x(z).π0)

  def midProjection[X <: ~, Y <: ~, Z <: ~](
    x: DOT[X],
    y: DOT[Y],
    z: DOT[Z]
  ): X x Y x Z > Y =
    x.x(y).π1.o(x.x(y).x(z).π0)

  def rightProjection[X <: ~, Y <: ~, Z <: ~](
    x: DOT[X],
    y: DOT[Y],
    z: DOT[Z]
  ): X x Y x Z > Z =
    x.x(y).x(z).π1

  class PartialArrowClassifier[A <: ~](
    dot: DOT[A]
  ) {
    val classifier =
      dot.power
        .forAll(dot, dot) { (f, a, b) =>
          implicit val anonImplicit = dot.power
          (f(a) ∧ f(b)) → dot.=?=(a, b)
      } whereTrue

    val include = classifier.restrict(dot.singleton)

    val ⏊ = classifier.restrict(dot.power.transpose(I)((i, x) => falsity(i)))

    def extend[S <: ~, B <: ~](
      monic: S > B,
      arrowOnSub: S > A
    ) =
      classifier.restrict(
        dot.power.transpose(
          monic.target
        ) { (t, a) =>
          monic.target.exists(
            monic.source
          ) { (t, s) =>
            dot.=?=(
              arrowOnSub(s),
              a
            ) ∧
              monic.target.=?=(
                monic(s),
                t
              )
          }(t)
        }
      )
  }

  class Coproduct[A <: ~, B <: ~](
    left: DOT[A],
    right: DOT[B]
  ) {
    private val fullProduct =
      left.pac.classifier.x(right.pac.classifier)

    private val injectLeftFull =
      left.pac.include.x(right.pac.⏊.o(left.toI))
    private val injectRightFull =
      (left.pac.⏊.o(right.toI)).x(right.pac.include)

    val coproduct = fullProduct.where(x => injectLeftFull.chi(x) ∨ injectRightFull.chi(x))
    val injectLeft = coproduct.restrict(injectLeftFull)
    val injectRight = coproduct.restrict(injectRightFull)

    def sum[X <: ~](
      leftArrow: A > X,
      rightArrow: B > X
    ) = {
      val target = leftArrow.target
      coproduct.arrowFromFunctionalRelation(target) { (αβ, x) =>
        coproduct.exists(left) { (αβ, a) =>
          coproduct.=?=(
            αβ,
            injectLeft(a)
          ) ∧ target.=?=(
            x,
            leftArrow(a)
          )
        }(αβ) ∨
          coproduct.exists(right) { (αβ, b) =>
            coproduct.=?=(
              αβ,
              injectRight(b)
            ) ∧ target.=?=(
              x,
              rightArrow(b)
            )
          }(αβ)
      }
    }
  }

  class Quotient[S <: ~](
    dot: DOT[S],
    equiv: Relation[S, S]
  ) {
    val arrow: S > QUOTIENT[S] =
      dot.power
        .transpose(
          dot
        )(
          equiv.criterion
        )
        .factorizeEpiMono
        ._1

    def lift[
      T <: ~
    ](
      compatibleArrow: S > T
    ): QUOTIENT[S] > T = {
      val target = compatibleArrow.target

      target.power.transpose(
        arrow.target
          .x(target)
          .existsMid(
            compatibleArrow.source
          ) { (q, s, t) =>
            implicit val anonImplicit = dot.power
            q(s) ∧ target.=?=(
              compatibleArrow(s),
              t
            )
          }
      ) \ target.singleton
    }
  }

  // TODO: shouldn't need this, hack to get round bug in Scala 2.12.0-M4
  def tempConst[A <: ~](dot: DOT[A])(a: A) =
    I(dot)(_ => a)
}

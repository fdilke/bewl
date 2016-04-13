package com.fdilke.bewl.topos

import com.fdilke.bewl.helper.{↔, Memoize}
import com.fdilke.bewl.topos.algebra.{AlgebraicConstructions, AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.topos.constructions.{ConstructToposOfAutomorphisms, ConstructToposOfGroupActions, ConstructToposOfMonoidActions}
import org.scalatest.Matchers

import scala.Function.tupled
import scala.language.{higherKinds, postfixOps}
import Matchers._

trait ToposAlgebra[~] extends
  AlgebraicMachinery[~] with
  AlgebraicConstructions[~] with
  AlgebraicStructures[~]

trait ToposConstructions[~] extends BaseTopos[~]
  with ConstructToposOfMonoidActions[~]
  with ConstructToposOfGroupActions[~]
  with ConstructToposOfAutomorphisms[~] {

  Ɛ: AlgebraicStructures[~] with
    AlgebraicMachinery[~] with
    LogicalOperations[~] =>
}

trait Topos[~] extends BaseTopos[~] with
  LogicalOperations[~] with
  ToposAlgebra[~] with
  ToposConstructions[~]

trait BaseTopos[~] { self: LogicalOperations[~] =>
  type DOT[S <: ~] <: Dot[S]
  type >[S <: ~, T <: ~] <: Arrow[S, T]

  type →[T <: ~, U <: ~] = (T => U) with ~
  type x[T <: ~, U <: ~] = (T, U) with ~

  type UNIT <: ~
  val I : DOT[UNIT]

  type TRUTH <: ~
  val omega: DOT[TRUTH]
  val truth: UNIT > TRUTH

  implicit class OmegaEnrichments(truthValue: TRUTH) {
    import TruthObject._
    def >(that: TRUTH) = implies(truthValue, that)
    def ^(that: TRUTH) = and(truthValue, that)
    def v(that: TRUTH) = or(truthValue, that)
  }

  type EXPONENTIAL[S <: ~, T <: ~] = ExponentialDot[S, T, S → T] with DOT[S → T]
  trait ExponentialDot[S <: ~, T <: ~, S_T <: (S => T) with ~] { dot: DOT[S_T] =>
    val source: DOT[S]
    val target: DOT[T]

    def transpose[R <: ~](biArrow: BiArrow[R, S, T]): R > S_T

    final def transpose[R <: ~](
      index: DOT[R]
    )(
      bifunc: (R, S) => T
    ): R > S_T =
      transpose(
        (index x source).biArrow(target)(bifunc)
      )

    final def evaluation: BiArrow[S_T, S, T] =
      (this x source).biArrow(target) { _(_) }
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
      final def universally[T <: ~](target: DOT[T])(bifunc: (L x R, T) => TRUTH) =
        BiArrow(hackedThis, hackedThis.forAll(target)(bifunc))
      final def existentially[T <: ~](target: DOT[T])(bifunc: (L x R, T) => TRUTH) =
        BiArrow(hackedThis, hackedThis.exists(target)(bifunc))
    }

  type EQUALIZER[S <: ~] = EqualizingDot[S] with DOT[S]
  trait EqualizingDot[S <: ~] { dot: DOT[S] =>
    val equalizerTarget: DOT[S]
    def restrict[R <: ~](arrow: R > S): R > S
    lazy val inclusion: S > S =
      dot(equalizerTarget) { s => s }
  }

  private object InitialDot {
    lazy val O = TruthObject.falsity whereTrue
    def fromO[X <: ~](dot: DOT[X]) = {
      val xO = dot.toTrue ?= (TruthObject.falsity o dot.toI)
      val xOtoO = O restrict xO.toI
      xO.inclusion o xOtoO.inverse
    }
  }
  type VOID = UNIT // TODO: fix with strict equalizers
  lazy val O: DOT[VOID] = InitialDot.O

  trait BaseDot[S <: ~] { self: DOT[S] =>
    val toI: S > UNIT
    val globals: Traversable[UNIT > S]
    def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T]
    def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T]
    def apply[T <: ~](target: DOT[T])(f: S => T) : S > T
    def sanityTest
  }

  trait Dot[S <: ~] extends BaseDot[S] { dot: DOT[S] =>

    final lazy val identity: S > S =
      dot(dot) { s => s }

    final private val memoizedProduct =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[U <: ~] = BIPRODUCT[S, U]}) # λ,
        ~
      ] (xUncached)

    final def x[U <: ~](that: DOT[U]): BIPRODUCT[S, U] = memoizedProduct(that)

    final private val memoizedExponential =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[T <: ~] = EXPONENTIAL[S, T]}) # λ,
        ~
      ] (`>Uncached`)

    final def >[T <: ~](that: DOT[T]): EXPONENTIAL[S, T] = memoizedExponential(that)

    final private val memoizedCoproduct =
      Memoize.generic.withLowerBound[
        DOT,
        ({ type λ[T <: ~] = Coproduct[S, T]}) # λ,
        ~
      ](`+Uncached`)
    final def ⊔[T <: ~](that: DOT[T]): Coproduct[S, T] = memoizedCoproduct(that)

    final lazy val toTrue = truth o toI
    final lazy val power = this > omega
    final lazy val ∀ = toTrue.name.chi
    final lazy val squared = dot x dot

    final def map(f: S => S) = dot(dot) { f }
    final def flatMap(bifunc: S => S > S) =
      (this x this).biArrow(this) {
        bifunc(_)(_)
      }

    final lazy val ∃ =
      power.forAll(omega) { (f, w) =>
          (power x omega).universally(dot) {
            case ((f, w), x) => f(x) > w
          }(f, w) > w
      }

    final def preForAll[R <: ~](source: DOT[R])(bifunc: (R, S) => TRUTH): R > TRUTH =
      ∀ o power.transpose(source)(bifunc)

    final def forAll[T <: ~](target: DOT[T])(g: (S, T) => TRUTH): S > TRUTH =
      target.preForAll(dot)(g)

    final def forAll[
      T <: ~, 
      U <: ~
    ] (
      target: DOT[T], 
      target2: DOT[U]
    ) (
      g: (S, T, U) => TRUTH
    ): S > TRUTH =
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
    ): S > TRUTH =
      forAll(target) { (x, t) =>
        target.forAll(target2) { (t, u) => 
          target2.forAll(target3) { (u, v) =>
              g(x, t, u, v)
            }(u)
          }(t)
        }

    final def preExists[R <: ~](source: DOT[R])(bifunc: (R, S) => TRUTH): R > TRUTH =
      ∃ o power.transpose(source)(bifunc)

    final def exists[T <: ~](target: DOT[T])(g: (S, T) => TRUTH): S > TRUTH =
      target.preExists(this)(g)

    final lazy val diagonal: S > (S x S) =
      this(squared) { x => squared.pair(x, x) }

    final lazy val =?= : BiArrow[S, S, TRUTH] =
      BiArrow(squared, diagonal.chi)

    final lazy val singleton: S > (S → TRUTH) =
      power transpose =?=

    final def >>[T <: ~](
      target: DOT[T]
    ): Traversable[
      S > T
    ] =
      (dot > target).globals map { global =>
        dot(target) { s =>
          global(dot toI s)(s)
        }}

    final lazy val fromO: VOID > S =
      InitialDot fromO dot

    final lazy val pac = new PartialArrowClassifier(dot)

    final def `+Uncached`[T <: ~](that: DOT[T]) =
      new Coproduct(this, that)

    final def +- [T <: ~](that: DOT[T]) =
      (this ⊔ that).injectLeft

    final def -+ [T <: ~](that: DOT[T]) =
      (this ⊔ that).injectRight

    def +[U <: ~](dash: DOT[U]) =
      (dot ⊔ dash).coproduct

    def arrowFromFunctionalRelation[B <: ~](
      target: DOT[B]
    )(
      predicate: (S, B) => TRUTH
    ): S > B = {
      val product = dot x target
      val graph = product(omega) {
        case (a, b) => predicate(a, b)
      }.whereTrue.inclusion
      (product.π1 o graph) / (product.π0 o graph)
    }

    lazy val doubleExpMonad =
      new Monad[
        ({type λ[X <: ~] = X → S → S}) # λ
      ] {
        override def apply[X <: ~](
          dash: DOT[X]
        ) =
          new At[X] {

            private lazy val doubleExp: EXPONENTIAL[X → S, S] =
              dash > dot > dot

            override lazy val free: DOT[X → S → S] =
              doubleExp

            override lazy val eta =
              doubleExp.transpose(dash) {
                (x, f) => f(x)
              }

            override lazy val mu =
                (dash > dot > dot).transpose(
                  dash > dot > dot > dot > dot
                ) {
                  (ffff, f) => ffff(
                    (dash > dot > dot > dot).transpose(
                      dash > dot
                    ) {
                      (x, f) => f(x)
                    }(
                      f
                    )
                  )
                }
          }

        override def map[X <: ~, Y <: ~](
          arrow: X > Y
        ) =
          dot > (dot > arrow)
      }

    // Contravariant exponential functor
    final def >[
      T <: ~,
      U <: ~
    ] (
      arrow: T > U
    ): (U → S) > (T → S) =
      (arrow.source > dot).transpose(
        arrow.target > dot
      ) { (s_u, t) =>
        s_u(arrow(t))
      }
  }

  trait Monad[M[X <: ~] <: ~] {
    // TODO: cache these automatically
    def apply[X <: ~](dot: DOT[X]): At[X]
    def map[X <: ~, Y <: ~](arrow: X > Y): M[X] > M[Y]

    trait At[X <: ~] {
      val free: DOT[M[X]]
      val eta: X > M[X]
      val mu: M[M[X]] > M[X]

      def sanityTest = {
//        mu o map(eta(dot)) shouldBe apply(dot).identity
//        mu o eta(apply(dot)) shouldBe apply(dot).identity
//        mu o map(mu(dot)) shouldBe (mu(dot) o mu(apply(dot)))
      }
    }

    type Algebra[X <: ~] = M[X] => X // TODO: sanity test
  }

  trait BaseArrow[S <: ~, T <: ~] {
    val source: DOT[S]
    val target: DOT[T]
    val chi: T > TRUTH

    def apply(s: S): T
    def ?=(that: S > T): EQUALIZER[S]
    def o[R <: ~](that: R > S) : R > T
    def \[U <: ~](monic: U > T) : S > U
    def sanityTest
  }

  trait Arrow[S <: ~, T <: ~] extends BaseArrow[S, T] { self: S > T =>
    final lazy val name =
      (source > target).transpose(I) {
          (i, x) => this(x)
      }

    final def x[U <: ~](that: S > U): S > (T x U) = {
      val product = target x that.target
      source(product) {
        s => product.pair(this(s), that(s))
      }
    }

    final def toBool(implicit eq: T =:= TRUTH): Boolean =
      this == source.toTrue

    final def whereTrue(implicit eq: T =:= TRUTH): EQUALIZER[S] =
      self.asInstanceOf[S > TRUTH] ?= source.toTrue

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

    final lazy val inverse: T > S =
      source.power.transpose(target) {
          (t, s) => target.=?=(t, this(s))  
        } \ source.singleton

    def /[R <: ~](iso: S > R) : R > T =
      self o iso.inverse

    def +[U <: ~](that: U > T) =
      (source ⊔ that.source).sum(this, that)
  }

  case class BiArrow[
    L <: ~,
    R <: ~,
    T <: ~
  ] (
    product: BIPRODUCT[L, R],
    arrow: L x R > T
  ) {
    def apply(l: L, r: R): T = arrow(product.pair(l, r))
    def apply[S <: ~](l: S > L, r: S > R): S > T = arrow o (l x r)
  }

  // Helper methods for triproducts (this could obviously be extended).
  def leftProjection[X <: ~, Y <: ~, Z <: ~](
    x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : X x Y x Z > X =
    (x x y).π0 o (x x y x z).π0

  def midProjection[X <: ~, Y <: ~, Z <: ~](
   x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : X x Y x Z > Y =
    (x x y).π1 o (x x y x z).π0

  def rightProjection[X <: ~, Y <: ~, Z <: ~](
   x: DOT[X], y: DOT[Y], z: DOT[Z]
  ) : X x Y x Z > Z =
    (x x y x z).π1

  class PartialArrowClassifier[A <: ~](dot: DOT[A]) {
    val classifier = dot.power.forAll(dot, dot) {
      (f, a, b) =>
        (f(a) ^ f(b)) > dot.=?=(a, b)
    } whereTrue

    val include = classifier restrict dot.singleton

    val ⏊ = classifier restrict
      dot.power.transpose(I) {
        (i, x) => TruthObject.falsity(i)
      }

    def extend[S <: ~, B <: ~](
      monic: S > B,
      arrowOnSub: S > A
    ) =
      classifier.restrict(
        dot.power.transpose(monic.target) {
          (t, a) => monic.target.exists(monic.source) {
            (t, s) =>
              dot.=?=(
                arrowOnSub(s), a
              ) ^
                monic.target.=?=(
                  monic(s), t
                )
          }(t)
        })
  }

  class Coproduct[A <: ~, B <: ~](left: DOT[A], right: DOT[B]) {
    private val fullProduct = left.pac.classifier x right.pac.classifier

    private val injectLeftFull = left.pac.include x (right.pac.⏊ o left.toI)
    private val injectRightFull = (left.pac.⏊ o right.toI) x right.pac.include

    val coproduct = fullProduct(omega) { x =>
      injectLeftFull.chi(x) v injectRightFull.chi(x)
    } whereTrue
    val injectLeft = coproduct restrict injectLeftFull
    val injectRight = coproduct restrict injectRightFull

    def sum[X <: ~](leftArrow: A > X, rightArrow: B > X) = {
      val target = leftArrow.target
      coproduct.arrowFromFunctionalRelation(target) {
        (αβ, x) =>
          coproduct.exists(left) {
            (αβ, a) => coproduct.=?=(αβ, injectLeft(a)) ^ target.=?=(x, leftArrow(a))
          }(αβ) v
          coproduct.exists(right) {
            (αβ, b) => coproduct.=?=(αβ, injectRight(b)) ^ target.=?=(x, rightArrow(b))
          }(αβ)
      }
    }
  }

  // TODO: machineries to help with the topos of monoid actions. Can these go somewhere else?
  trait ElementWrapper[
    A <: ~
  ] {
    val element: A
  }

  object VanillaWrapper {
    def ↔[A <: ~] = new ↔[A, VanillaWrapper[A]](
      a => VanillaWrapper(a),
      aa => aa.element
    )
  }

  case class VanillaWrapper[
    A <: ~
  ] (
    element: A
  ) extends ElementWrapper[A]
}

trait Wrappings[
  ~,
  BASE,
  PREDOT[_ <: BASE],
  PREARROW[_ <: BASE, _ <: BASE],
  WRAPPER[T <: BASE] <: ~
] { topos: BaseTopos[~] =>

  def makeDot[T <: BASE](predot: PREDOT[T]) : DOT[WRAPPER[T]]
  def makeArrow[S <: BASE, T <: BASE](prearrow: PREARROW[S, T]) : WRAPPER[S] > WRAPPER[T]
  def functionAsArrow[S <: BASE, T <: BASE](
    source: DOT[WRAPPER[S]], 
    target: DOT[WRAPPER[T]], 
    f: S => T
  ): WRAPPER[S] > WRAPPER[T]
  def bifunctionAsBiArrow[L <: BASE, R <: BASE, T <: BASE] (
    left: DOT[WRAPPER[L]],
    right: DOT[WRAPPER[R]],
    target: DOT[WRAPPER[T]]
  ) (
    bifunc: (L, R) => T
  ): BiArrow[WRAPPER[L], WRAPPER[R], WRAPPER[T]]

  final def bifunctionAsBiArrow[X <: BASE] (
    dot: DOT[WRAPPER[X]]
  ) (
     bifunc: (X, X) => X
  ): BiArrow[WRAPPER[X], WRAPPER[X], WRAPPER[X]] =
    bifunctionAsBiArrow[X, X, X](dot, dot, dot) { bifunc }

  // An implementation may have the ability to "unwrap" dots
  def unwrap[T <: BASE](dot: DOT[WRAPPER[T]]): PREDOT[T] =
    ???

  def burst[M[_], T <: BASE](
    bubble: M[WRAPPER[T]]
  )(implicit ev: WRAPPER[T] =:= T) : M[T] =
    bubble.asInstanceOf[M[T]]
}

object Wrappings {
  type NO_WRAPPER[T] = T
}

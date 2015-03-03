package com.fdilke.bewl.actions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.↔

// Monoids and monoid actions defined as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { Ɛ: BaseTopos with AlgebraicMachinery with LogicalOperations =>

  case class ElementProxy0[A <: ELEMENT](element: A)

  case class NaiveMonoid[M <: ELEMENT](carrier: STAR[M], unit: NullaryOp[M], multiply: BinaryOp[M]) {
    def sanityTest {
      // check the left unit law
      if (carrier(carrier) {
        x => multiply(unit(carrier.toI(x)), x)
      } != carrier.identity)
        throw new IllegalArgumentException("Left unit law for * with unit 1")

      // check the right unit law
      if (carrier(carrier) {
        x => multiply(x, unit(carrier.toI(x)))
      } != carrier.identity)
        throw new IllegalArgumentException("Right unit law for * with unit 1")

      // check the associative law
      if ((carrier x carrier x carrier)(carrier) {
        case ((x, y), z) => multiply(x, multiply(y, z))
      } != (carrier x carrier x carrier)(carrier) {
        case ((x, y), z) => multiply(multiply(x, y), z)
      })
        throw new IllegalArgumentException("Associative law for *")
    }

    def rightAction[A <: ELEMENT](actionCarrier: STAR[A])(actionMultiply: (A, M) => A) =
      new RightAction[A](actionCarrier, actionMultiply)

    lazy val rightRegularAction = 
      rightAction(carrier) { multiply(_, _) }

    lazy val rightActions = new RightMonoidActionsInDraft3

    case class RightAction[A <: ELEMENT] (
      actionCarrier: STAR[A],
      actionMultiply: (A, M) => A
    ) {
      def isMorphism[B <: ELEMENT](that: RightAction[B], quiver: QUIVER[A, B]) = 
        (quiver.source == this.actionCarrier) &&
        (quiver.target == that.actionCarrier) && (
          carrier.forAll(actionCarrier) { (x, m) =>
            that.actionCarrier.diagonal(
              quiver(this.actionMultiply(x, m)),
              that.actionMultiply(quiver(x), m)
            )
          } == actionCarrier.toTrue
        )
      def sanityTest = {
        // check the right unit law
        if (actionCarrier(actionCarrier) {
          a => actionMultiply(a, unit(actionCarrier.toI(a)))
        } != actionCarrier.identity)
          throw new IllegalArgumentException("Right unit law for * with unit 1")

        // check the associative law
        if ((actionCarrier x carrier x carrier)(actionCarrier) {
          case ((a, x), y) => actionMultiply(a, multiply(x, y))
        } != (actionCarrier x carrier x carrier)(actionCarrier) {
          case ((a, x), y) => actionMultiply(actionMultiply(a, x), y)
        })
          throw new IllegalArgumentException("Associative law for monoid action *")
      }
    }

    case class RightActionPrequiver[S <: ELEMENT, T <: ELEMENT](
      source: RightAction[S],
      target: RightAction[T],
      function: S => T
    )

/*
    class RightMonoidActionsInDraft2 extends Topos with 
      Wrappings[Ɛ.ELEMENT, RightAction, RightActionPrequiver] {

      trait ElementWrapper[A <: Ɛ.ELEMENT, AA <: ElementWrapper[A, AA]] { wrapper =>
        // final type BASE = A
        val element: A

        // def apply[F, G](f: F, g: G): (F, G) with ElementWrapper[A, AA] =
        //   new (F, G)(f, g) with ElementWrapper[A, AA] {
        //     override val element = wrapper.element
        //   }

        // def apply[F, G](f2g: F => G): (F => G) with ElementWrapper[A, AA] =
        //   new (F => G) with ElementWrapper[A, AA] {
        //     def apply(f: F): G = f2g(f)
        //     override val element = wrapper.element
        //   }

        // type PREBIPRODUCT[ZZ <: ELEMENT] = ZZ#POSTBIPRODUCT[A, AA]
        // type POSTBIPRODUCT[B <: Ɛ.ELEMENT, BB <: ElementWrapper[B, BB]] = BiproductWrapper[A, AA, B, BB]

        // type PREBIPRODUCT[ZZ <: ELEMENT] = ZZ#POSTBIPRODUCT[A, AA] 
        // type POSTBIPRODUCT[B <: Ɛ.ELEMENT, BB <: ElementWrapper[B, BB]] = H forSome {
        //   type H <: ElementWrapper[Ɛ.x[A, B], H]
        // }
        type PREBIPRODUCT[ZZ <: ELEMENT] = ZZ#POSTBIPRODUCT[A, AA]
        type POSTBIPRODUCT[B <: Ɛ.ELEMENT, BB <: ElementWrapper[B, BB]] = BiproductWrapper[A, AA, B, BB]
        // H forSome {
        //   type H <: ElementWrapper[Ɛ.x[A, B], H]
        // }
      }

      class BiproductWrapper[
        A <: Ɛ.ELEMENT,
        AA <: ElementWrapper[A, AA],
        B <: Ɛ.ELEMENT,
        BB <: ElementWrapper[B, BB]
      ] (
        aa: AA,
        bb: BB,
        aXb: Ɛ.x[A, B]
      ) extends (AA, BB)(aa, bb) with ElementWrapper[
        Ɛ.x[A, B],
        BiproductWrapper[A, AA, B, BB]
      ] {
        override val element = aXb
      }

      class VanillaWrapper[A <: Ɛ.ELEMENT](a: A) extends
        ElementWrapper[A, VanillaWrapper[A]] {
            override val element = a
      }

      override type ELEMENT = TT forSome {
        type TT <: ElementWrapper[_ <: Ɛ.ELEMENT, TT]
      }
      override type STAR[AA <: ELEMENT] = ActionStarFacade[AA] 
      override type QUIVER[AA <: ELEMENT, BB <: ELEMENT] = RightActionQuiverFacade[AA, BB]
      override type UNIT = VanillaWrapper[Ɛ.UNIT]

      override type x[SS <: ELEMENT, TT <: ELEMENT] = (SS, TT) with TT#PREBIPRODUCT[SS]

      override type >[SS <: ELEMENT, TT <: ELEMENT] = (SS => TT) with ELEMENT

      type RIGHT_IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = VanillaWrapper[RIGHT_IDEAL]
      override val I: RightActionStar[Ɛ.UNIT, UNIT] = RightActionStar(Ɛ.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) => Ɛ.OmegaEnrichments(f(m)) > f(multiply(m, n))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal

        def restrict[H <: Ɛ.ELEMENT](that: Ɛ.STAR[H])(bifunc: (H, M) => Ɛ.TRUTH): Ɛ.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }
        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            Ɛ.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }
      override lazy val omega = RightIdeals.omega
      override lazy val truth: QUIVER[UNIT, TRUTH] = 
        new RightActionQuiver[Ɛ.UNIT, UNIT, RIGHT_IDEAL, TRUTH](I, omega, // TODO: need generics?
          carrier.power.transpose((Ɛ.I x carrier).biQuiver(Ɛ.omega) {
            case (x, m) => Ɛ.truth(x)
        }))

      trait ActionStarFacade[AA <: ELEMENT] extends Star[AA] {
        def preMultiplyUncached[
          Z <: Ɛ.ELEMENT, 
          ZZ <: ElementWrapper[Z, ZZ]
        ] (
          pre: RightActionStar[Z, ZZ]
        ): BIPRODUCT[ZZ, AA]

        def preExponentiateUncached[
          Z <: Ɛ.ELEMENT, 
          ZZ <: ElementWrapper[Z, ZZ]
        ] (
          pre: RightActionStar[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA]

        def preApply[
          Z <: Ɛ.ELEMENT, 
          ZZ <: ElementWrapper[Z, ZZ]
        ] (
          pre: RightActionStar[Z, ZZ]
        )(
          f: ZZ => AA
        ): QUIVER[ZZ, AA]
      }

      trait WeakBiproductStar[L <: ELEMENT, R <: ELEMENT, LXR <: L x R] extends ActionStarFacade[LXR] { star: STAR[LXR] =>
        val left: STAR[L]
        val right: STAR[R]
        def pair(l: L, r: R): L x R
      }

      class RightActionStar[A <: Ɛ.ELEMENT, AA <: ElementWrapper[A, AA]](
        private[RightMonoidActionsInDraft2] val action: RightAction[A],
        private val ^ : A => AA
      ) extends ActionStarFacade[AA] { star =>
        def v(aa: AA): A = aa.element // TODO: inline / optimize away?
        private lazy val pairs: Ɛ.BIPRODUCT[M, A] = carrier x action.actionCarrier 

        override val toI: QUIVER[AA, UNIT] = 
          new RightActionQuiver[A, AA, Ɛ.UNIT, UNIT](this, I, action.actionCarrier.toI) // TODO: need generics?

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[BB <: ELEMENT](that: STAR[BB]) = that.preMultiplyUncached(this)

        override def preMultiplyUncached[Z <: Ɛ.ELEMENT, ZZ <: ElementWrapper[Z, ZZ]](
          pre: RightActionStar[Z, ZZ]
        ): BIPRODUCT[ZZ, AA] = {
          val product: Ɛ.BIPRODUCT[Z, A] = pre.action.actionCarrier x action.actionCarrier           

      val weezy =
       new RightActionStar[
          Ɛ.x[Z, A],
          BiproductWrapper[Z, ZZ, A, AA] // ZZ x AA
        ] (
          rightAction(product){
            case ((z, a), m) => product.pair(
                pre.action.actionMultiply(z, m),
                action.actionMultiply(a, m) 
              )},
          (zxa: Ɛ.x[Z, A]) =>
            zxa match { case (z, a) =>
              val zz: ZZ = pre ^ z
              val aa: AA = star ^ a
              new BiproductWrapper[Z, ZZ, A, AA](zz, aa, zxa) // .asInstanceOf[x[ZZ, AA]] // TODO can fix!!
            }
        ) with WeakBiproductStar[ZZ, AA, BiproductWrapper[Z, ZZ, A, AA]] {               
          override val left: STAR[ZZ] = pre
          override val right: STAR[AA] = star
          override def pair(zz: ZZ, aa: AA): x[ZZ, AA] = { 
            val z: Z = zz.element
            val a: A = aa.element
            new BiproductWrapper[Z, ZZ, A, AA](zz, aa, product.pair(z, a)).asInstanceOf[x[ZZ, AA]] // TODO can fix!!
        }}
        weezy.asInstanceOf[BIPRODUCT[ZZ, AA]]
          
          // new RightActionStar[Ɛ.x[ZZ#BASE, AA#BASE], ZZ x AA] (
          //     rightAction(product){
          //       case ((z, a), m) => product.pair(
          //           pre.action.actionMultiply(z, m),
          //           action.actionMultiply(a, m) 
          //         )},
          //     (zxa: Ɛ.x[Z, A]) =>
          //       zxa match { case (z, a) =>
          //         val zz: ZZ = pre ^ z
          //         val aa: AA = star ^ a
          //         val zxaWrapped: Ɛ.ElementWrapper[Ɛ.x[zz.BASE, aa.BASE]] = Ɛ.ElementWrapper(zxa)
          //         zxaWrapped(zz, aa).asInstanceOf[ZZ x AA] // TODO: fix with reveal?
          //       }
          // ) with BiproductStar[ZZ, AA] {               
          //   override val left: STAR[ZZ] = pre
          //   override val right: STAR[AA] = star
          //   override def pair(zz: ZZ, aa: AA): x[ZZ, AA] = { 
          //     val z: Z = zz.element
          //     val a: A = aa.element
          //     Ɛ.ElementWrapper(product.pair(z, a))(zz, aa).asInstanceOf[ZZ x AA] // TODO: fix with reveal?
          // }}
          
          }

        override def `>Uncached`[BB <: ELEMENT](that: STAR[BB]): EXPONENTIAL[AA, BB] = 
          that.preExponentiateUncached(this)

        override def preExponentiateUncached[Z <: Ɛ.ELEMENT, ZZ <: ElementWrapper[Z, ZZ]](
          pre: RightActionStar[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA] = {
          val mXz = pre.pairs
          val possibleMorphisms = mXz > action.actionCarrier
          val isMorphism = (mXz x carrier).forAll(possibleMorphisms) {
            case (f, ((m, z), n)) =>
              action.actionCarrier.diagonal(
                f(mXz.pair(multiply(m, n), pre.action.actionMultiply(z, n))),
                action.actionMultiply(f(mXz.pair(m, z)), n)
              )
          }
          type P = Ɛ.>[Ɛ.x[M, Z],A]
          val morphisms: Ɛ.EQUALIZER[P] = possibleMorphisms.toTrue ?= isMorphism
          val morphismMultiply = morphisms.restrict(possibleMorphisms.transpose(
            (morphisms x carrier x mXz).biQuiver(action.actionCarrier) {
              case ((f, m), (n, z)) => morphisms.inclusion(f)(
                mXz.pair(multiply(m, n), z)
            )}))

          // new RightActionStar[P, ZZ > AA](
          //   rightAction(morphisms) {
          //     Ɛ.BiQuiver(morphisms x carrier, morphismMultiply)(_,_)
          //   },
          //   (p: P) => new VanillaWrapper(p) { 
          //     (zz: ZZ) => {
          //       val z = zz.element
          //       val unitM: M = unit(pre.action.actionCarrier.toI(z))
          //       val a: A = p(mXz.pair(unitM, z))
          //       star ^ a
          //   }}.asInstanceOf[ZZ > AA] // TODO: fix these casts dammit
          // ) with ExponentialStar[ZZ, AA] { exponentialStar =>
          //   val source: STAR[ZZ] = pre
          //   val target: STAR[AA] = star
          //   def transpose[RR <: ELEMENT](biQuiver: BiQuiver[RR, ZZ, AA]): QUIVER[RR, ZZ > AA] = 
          //     biQuiver.product.left.calcTranspose[Z, ZZ, A, AA](
          //       pre, star, morphisms, possibleMorphisms, exponentialStar, biQuiver
          //     )}

              null.asInstanceOf[EXPONENTIAL[ZZ, AA]]
            }

        private def calcTranspose[
          R <: Ɛ.ELEMENT, 
          RR <: ElementWrapper[R, RR], 
          T <: Ɛ.ELEMENT, 
          TT <: ElementWrapper[T, TT] 
        ] (
          source: RightActionStar[R, RR],
          target: RightActionStar[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialStar: RightActionStar[Ɛ.>[Ɛ.x[M, R], T], RR > TT],
          biQuiver: BiQuiver[AA, RR, TT]
        ): QUIVER[AA, RR > TT] = {
          type P = Ɛ.>[Ɛ.x[M, R],T]
          val innerQuiver: Ɛ.QUIVER[A, P] =
            morphisms.restrict(possibleMorphisms.transpose(
              (action.actionCarrier x source.pairs).biQuiver(target.action.actionCarrier) {
                case (a, (m, r)) => 
                  val aa: AA = star ^ action.actionMultiply(a, m)
                  val rr: RR = source ^ r
                  target.v(biQuiver(aa, rr))
                }))
          this(exponentialStar) { aa =>
            val p: P = innerQuiver(this v aa)
            exponentialStar ^ p
          }              
        }

        override def apply[BB <: ELEMENT](that: STAR[BB])(f: AA => BB): QUIVER[AA, BB] =
          that.preApply(this)(f)

        override def preApply[Z <: Ɛ.ELEMENT, ZZ <: ElementWrapper[Z, ZZ]](
          pre: RightActionStar[Z, ZZ]
        )(
          f: ZZ => AA
        ): QUIVER[ZZ, AA] = 
          new RightActionQuiver[Z, ZZ, A, AA](pre, star, // TODO: need generics?
            pre.action.actionCarrier(star.action.actionCarrier) { z =>
              val zz = pre ^ z
              val aa = f(zz)
              star v aa    // TODO: condense
            })

        override def toString = "RightActionStar[" + action.actionCarrier + "]"
      }

      object RightActionStar {
        def apply[A <: Ɛ.ELEMENT](actionCarrier: Ɛ.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar[A, VanillaWrapper[A]](
            rightAction(actionCarrier)(actionMultiply),
            (a: A) => new VanillaWrapper(a)
          ) 
      }

      // TODO: will presumably be fleshed out as I populate the quiver API
      trait RightActionQuiverFacade[E <: ELEMENT, F <: ELEMENT] extends
        Quiver[E, F] { facade =>
      }

      class RightActionQuiver[
        A <: Ɛ.ELEMENT, 
        AA <: ElementWrapper[A, AA], 
        B <: Ɛ.ELEMENT, 
        BB <: ElementWrapper[B, BB]
      ] (
        val source: RightActionStar[A, AA],
        val target: RightActionStar[B, BB],
        val quiver: Ɛ.QUIVER[A, B]
      ) extends RightActionQuiverFacade[AA, BB] {

        override lazy val chi: QUIVER[BB, TRUTH] = null

        override def \[UU <: ELEMENT](monic: QUIVER[UU, BB]) = null

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[AA, BB]): EQUALIZER[AA] = null

        override def apply(e: AA) = null.asInstanceOf[BB]

        override def o[ZZ <: ELEMENT](that: QUIVER[ZZ, AA]): QUIVER[ZZ, BB] = null

        override def toString = "RightActionQuiver[" + quiver + "]"

        override def equals(other: Any): Boolean = false
        override def hashCode = 0
      }

      override type WRAPPER[T <: Ɛ.ELEMENT] = VanillaWrapper[T]

      private val memoizedStarWrapper = {
        type STAR_WRAPPER[T <: Ɛ.ELEMENT] = STAR[WRAPPER[T]]
        def wrap[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR_WRAPPER[T] =
          null // new RightActionStar[T, WRAPPER[T]](input)
        Memoize.generic.withLowerBound[RightAction, STAR_WRAPPER, Ɛ.ELEMENT](wrap)
      }

      override def star[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR[WRAPPER[T]] =
        null // memoizedStarWrapper(input)

      override def quiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
        source: STAR[WRAPPER[S]], 
        target: STAR[WRAPPER[T]], 
        f: S => T
      ): QUIVER[WRAPPER[S], WRAPPER[T]] = {
        null
        // val src = source.asInstanceOf[RightActionStar[S, WRAPPER[S]]]
        // val tgt = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        // new RightActionQuiver[S, WRAPPER[S], T, WRAPPER[T]](
        //   src, 
        //   tgt, 
        //   src.action.actionCarrier(tgt.action.actionCarrier) { f })
      }

      override def bifunctionAsBiQuiver[
        L <: Ɛ.ELEMENT, 
        R <: Ɛ.ELEMENT, 
        T <: Ɛ.ELEMENT
      ] (
        left: STAR[WRAPPER[L]],
        right: STAR[WRAPPER[R]],
        target: STAR[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]] = {
        null
        // val l =   left.asInstanceOf[RightActionStar[L, WRAPPER[L]]]
        // val r =   left.asInstanceOf[RightActionStar[R, WRAPPER[R]]]
        // val t = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        // (l x r).biQuiver(t) { (a, b) =>
        //   ElementProxy0(bifunc(a.element, b.element))
        // }
      }
    }
*/    

    class RightMonoidActionsInDraft3 extends Topos with 
      Wrappings[Ɛ.ELEMENT, RightAction, RightActionPrequiver] {

      trait AbstractElementWrapper
      trait ElementWrapper[A <: Ɛ.ELEMENT] extends AbstractElementWrapper {
        val element: A
      }

      object VanillaWrapper {
        def ↔[A <: Ɛ.ELEMENT] = new ↔[A, VanillaWrapper[A]](
          a => VanillaWrapper(a),
          aa => aa.element
        )
        def apply[A <: Ɛ.ELEMENT](a: A) =
          new VanillaWrapper(a)
      }
      class VanillaWrapper[A <: Ɛ.ELEMENT](val element: A) extends ElementWrapper[A] 

      class BiproductWrapper[
        A <: Ɛ.ELEMENT,
        AA <: ELEMENT,
        B <: Ɛ.ELEMENT,
        BB <: ELEMENT
      ] (
        aa: AA,
        bb: BB,
        aXb: Ɛ.x[A, B]
      ) extends (AA, BB)(aa, bb) with ElementWrapper[
        Ɛ.x[A, B]
      ] {
        override val element = aXb
      }

      class ExponentialWrapper[
        A <: Ɛ.ELEMENT,
        AA <: ELEMENT,
        B <: Ɛ.ELEMENT,
        BB <: ELEMENT
      ] (
        ma2b: Ɛ.>[Ɛ.x[M, A], B],
        aa2bb: AA => BB
      ) extends (AA => BB) with ElementWrapper[
        Ɛ.>[Ɛ.x[M, A], B]
      ] {
        def apply(aa: AA): BB = aa2bb(aa)

        override val element = ma2b
      }

      override type ELEMENT = AbstractElementWrapper
      
      override type STAR[AA <: ELEMENT] = ActionStarFacade[AA] 
      override type QUIVER[AA <: ELEMENT, BB <: ELEMENT] = RightActionQuiverFacade[AA, BB]
      override type UNIT = VanillaWrapper[Ɛ.UNIT]

      // TODO: nothing's being overridden: simplify this. OR sort it out properly!
      override type x[SS <: ELEMENT, TT <: ELEMENT] = (SS, TT) with ELEMENT

      override type >[SS <: ELEMENT, TT <: ELEMENT] = (SS => TT) with ELEMENT

      type RIGHT_IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = VanillaWrapper[RIGHT_IDEAL]
      override val I: RightActionStar[Ɛ.UNIT, UNIT] = RightActionStar(Ɛ.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        // private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
        //   case (f, (m, n)) => Ɛ.OmegaEnrichments(f(m)) > f(multiply(m, n))
        // }

        private val isIdeal = carrier.forAll(possibleIdeals) {
          case (f, m) => carrier.forAll(carrier) {
              case (m, n) => Ɛ.OmegaEnrichments(f(m)) > f(multiply(m, n))
            }(m)
        }

        private val ideals = possibleIdeals.toTrue ?= isIdeal

        def restrict[H <: Ɛ.ELEMENT](
          that: Ɛ.STAR[H]
        ) (
          bifunc: (H, M) => Ɛ.TRUTH
        ): Ɛ.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }
        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            Ɛ.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }
      override lazy val omega = RightIdeals.omega
      override lazy val truth: QUIVER[UNIT, TRUTH] = 
        new RightActionQuiver[Ɛ.UNIT, UNIT, RIGHT_IDEAL, TRUTH](I, omega, // TODO: need generics?
          carrier.power.transpose((Ɛ.I x carrier).biQuiver(Ɛ.omega) {
            case (x, m) => Ɛ.truth(x)
        }))

      trait ActionStarFacade[AA <: ELEMENT] extends Star[AA] {
        def preMultiplyUncached[
          Z <: Ɛ.ELEMENT, 
          ZZ <: ELEMENT
        ] (
          pre: RightActionStar[Z, ZZ]
        ): BIPRODUCT[ZZ, AA]

        def preExponentiateUncached[
          Z <: Ɛ.ELEMENT, 
          ZZ <: ELEMENT
        ] (
          pre: RightActionStar[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA]

        def preApply[
          Z <: Ɛ.ELEMENT, 
          ZZ <: ELEMENT
        ] (
          pre: RightActionStar[Z, ZZ]
        )(
          f: ZZ => AA
        ): QUIVER[ZZ, AA]

        def calcTranspose[
          R <: Ɛ.ELEMENT, 
          RR <: ELEMENT, 
          T <: Ɛ.ELEMENT, 
          TT <: ELEMENT 
        ] (
          source: RightActionStar[R, RR],
          target: RightActionStar[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialStar: RightActionStar[Ɛ.>[Ɛ.x[M, R], T], ExponentialWrapper[R, RR, T, TT]],
          biQuiver: BiQuiver[AA, RR, TT]
        ): QUIVER[AA, ExponentialWrapper[R, RR, T, TT]]

        def crossPreRestrict[
          Z <: Ɛ.ELEMENT,
          ZZ <: ELEMENT,
          A <: Ɛ.ELEMENT
        ] (
          source: RightActionStar[Z, ZZ],
          restrictedQuiver: Ɛ.QUIVER[Z, A]
        ): QUIVER[ZZ, AA]
      }

      class RightActionStar[A <: Ɛ.ELEMENT, AA <: ELEMENT](
        val action: RightAction[A],
        val ↔ : A ↔ AA
      ) extends ActionStarFacade[AA] { star =>
        def v(aa: AA): A = ↔ \ aa // TODO: inline / optimize away?
        private lazy val pairs: Ɛ.BIPRODUCT[M, A] = carrier x action.actionCarrier 

        override val toI: QUIVER[AA, UNIT] = 
          new RightActionQuiver[A, AA, Ɛ.UNIT, UNIT](this, I, action.actionCarrier.toI) // TODO: need generics?

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[BB <: ELEMENT](that: STAR[BB]) = that.preMultiplyUncached(this)

        override def preMultiplyUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z, ZZ]
        ): BIPRODUCT[ZZ, AA] = {
          val product: Ɛ.BIPRODUCT[Z, A] = pre.action.actionCarrier x action.actionCarrier           
          new RightActionStar[
              Ɛ.x[Z, A],
              BiproductWrapper[Z, ZZ, A, AA]
            ] (
              rightAction(product){
                case ((z, a), m) => product.pair(
                    pre.action.actionMultiply(z, m),
                    action.actionMultiply(a, m) 
                  )},
                new ↔[Ɛ.x[Z, A], BiproductWrapper[Z, ZZ, A, AA]] (
                  zxa => zxa match { case (z, a) =>
                    val zz: ZZ = pre.↔ / z
                    val aa: AA = star.↔ / a
                    new BiproductWrapper[Z, ZZ, A, AA](zz, aa, zxa)
                  },
                  zzXaa => zzXaa.element
                )
            ) with BiproductStar[ZZ, AA, BiproductWrapper[Z, ZZ, A, AA]] {               
              override val left: STAR[ZZ] = pre
              override val right: STAR[AA] = star
              override def pair(zz: ZZ, aa: AA) = { 
                val z: Z = pre.↔ \ zz
                val a: A = star.↔ \ aa
                new BiproductWrapper[Z, ZZ, A, AA](zz, aa, product.pair(z, a))
            }}.asInstanceOf[BIPRODUCT[ZZ, AA]]
          }

        override def `>Uncached`[BB <: ELEMENT](that: STAR[BB]): EXPONENTIAL[AA, BB] = 
          that.preExponentiateUncached(this)

        override def preExponentiateUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA] = {
          val mXz = pre.pairs
          val possibleMorphisms = mXz > action.actionCarrier
          // val isMorphism = (mXz x carrier).forAll(possibleMorphisms) {
          //   case (f, ((m, z), n)) =>
          //     action.actionCarrier.diagonal(
          //       f(mXz.pair(multiply(m, n), pre.action.actionMultiply(z, n))),
          //       action.actionMultiply(f(mXz.pair(m, z)), n)
          //     )
          // }
          // TODO: variadic version of forAll() with arguments the right way round

          val isMorphism = carrier.forAll(possibleMorphisms) {
            (f, n) => mXz.forAll(carrier) {
                case (n, (m, z)) => 
                  action.actionCarrier.diagonal(
                    f(mXz.pair(multiply(m, n), pre.action.actionMultiply(z, n))),
                    action.actionMultiply(f(mXz.pair(m, z)), n)
                  )
              }(n)
          }

          type P = Ɛ.>[Ɛ.x[M, Z],A]
          val morphisms: Ɛ.EQUALIZER[P] = possibleMorphisms.toTrue ?= isMorphism
          val morphismMultiply = morphisms.restrict(possibleMorphisms.transpose(
            (morphisms x carrier x mXz).biQuiver(action.actionCarrier) {
              case ((f, m), (n, z)) => morphisms.inclusion(f)(
                mXz.pair(multiply(m, n), z)
            )}))

          new RightActionStar[P, ExponentialWrapper[Z, ZZ, A, AA]](
            rightAction(morphisms) {
              Ɛ.BiQuiver(morphisms x carrier, morphismMultiply)(_,_)
            },
            new ↔[P, ExponentialWrapper[Z, ZZ, A, AA]](
              p => new ExponentialWrapper[Z, ZZ, A, AA](p,
                (zz: ZZ) => {
                  val z = pre.↔ \ zz
                  val unitM: M = unit(pre.action.actionCarrier.toI(z))
                  val a: A = p(mXz.pair(unitM, z))
                  star.↔ / a
                }
              ),
              zz2aa => zz2aa.element
            )) with ExponentialStar[ZZ, AA, ExponentialWrapper[Z, ZZ, A, AA]] { exponentialStar =>
            val source: STAR[ZZ] = pre
            val target: STAR[AA] = star
            def transpose[RR <: ELEMENT](biQuiver: BiQuiver[RR, ZZ, AA]): QUIVER[RR, ExponentialWrapper[Z, ZZ, A, AA]] = 
              biQuiver.product.left.calcTranspose[Z, ZZ, A, AA](
                pre, star, morphisms, possibleMorphisms, exponentialStar, biQuiver
              )
          }.asInstanceOf[EXPONENTIAL[ZZ, AA]]
        }

        override def calcTranspose[
          R <: Ɛ.ELEMENT, 
          RR <: ELEMENT, 
          T <: Ɛ.ELEMENT, 
          TT <: ELEMENT 
        ] (
          source: RightActionStar[R, RR],
          target: RightActionStar[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialStar: RightActionStar[Ɛ.>[Ɛ.x[M, R], T], ExponentialWrapper[R, RR, T, TT]],
          biQuiver: BiQuiver[AA, RR, TT]
        ): QUIVER[AA, ExponentialWrapper[R, RR, T, TT]] = {
          type P = Ɛ.>[Ɛ.x[M, R],T]
          val innerQuiver: Ɛ.QUIVER[A, P] =
            morphisms.restrict(possibleMorphisms.transpose(
              (action.actionCarrier x source.pairs).biQuiver(target.action.actionCarrier) {
                case (a, (m, r)) => 
                  val aa: AA = star.↔ / action.actionMultiply(a, m)
                  val rr: RR = source.↔ / r
                  target.v(biQuiver(aa, rr))
                }))
          this(exponentialStar) { aa =>
            val p: P = innerQuiver(this v aa)
            exponentialStar.↔ / p
          }              
        }

        override def apply[BB <: ELEMENT](that: STAR[BB])(f: AA => BB): QUIVER[AA, BB] =
          that.preApply(this)(f)

        override def preApply[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z, ZZ]
        )(
          f: ZZ => AA
        ): QUIVER[ZZ, AA] = 
          new RightActionQuiver[Z, ZZ, A, AA](pre, star, // TODO: need generics?
            pre.action.actionCarrier(star.action.actionCarrier) { z =>
              val zz = pre.↔ / z
              val aa = f(zz)
              star v aa    // TODO: condense
            })

        override def crossPreRestrict[
          Z <: Ɛ.ELEMENT,
          ZZ <: ELEMENT,
          AAA <: Ɛ.ELEMENT
        ] (
          source: RightActionStar[Z, ZZ],
          restrictedQuiver: Ɛ.QUIVER[Z, AAA]
        ): QUIVER[ZZ, AA] = 
          new RightActionQuiver[Z, ZZ, A, AA](
            source, 
            star,
            restrictedQuiver.asInstanceOf[Ɛ.QUIVER[Z, A]]
          )

        override def toString = "RightActionStar[" + action.actionCarrier + "]"
      }

      object RightActionStar {
        def apply[A <: Ɛ.ELEMENT](actionCarrier: Ɛ.STAR[A])(actionMultiply: (A, M) => A) =
          wrap(rightAction(actionCarrier)(actionMultiply))

        def wrap[A <: Ɛ.ELEMENT](action: RightAction[A]) =
          new RightActionStar[A, VanillaWrapper[A]](
            action,
            VanillaWrapper.↔[A]
          ) 
      }

      trait RightActionQuiverFacade[
        AA <: ELEMENT, 
        BB <: ELEMENT
      ] extends Quiver[AA, BB] { 

        def preBackDivide[
          B <: Ɛ.ELEMENT, 
          Z <: Ɛ.ELEMENT, 
          ZZ <: ELEMENT
        ] (
          pre: RightActionQuiver[Z, ZZ, B, BB]
        ): QUIVER[ZZ, AA]

        def preEqualizer[
          A <: Ɛ.ELEMENT, 
          B <: Ɛ.ELEMENT
        ] (pre: RightActionQuiver[A, AA, B, BB]): EQUALIZER[AA]

        def preRestrict[B <: Ɛ.ELEMENT](
          equalizingStar: EQUALIZER[BB],
          thunkedEqualizer: Ɛ.EQUALIZER[B]
        ): QUIVER[AA, BB]

        def preCompose[
          B <: Ɛ.ELEMENT, 
          C <: Ɛ.ELEMENT, 
          CC <: ELEMENT
        ] (pre: RightActionQuiver[B, BB, C, CC]): QUIVER[AA, CC]
      }

      class RightActionQuiver[
        A <: Ɛ.ELEMENT, 
        AA <: ELEMENT, 
        B <: Ɛ.ELEMENT, 
        BB <: ELEMENT
      ] (
        val source: RightActionStar[A, AA],
        val target: RightActionStar[B, BB],
        val quiver: Ɛ.QUIVER[A, B]
      ) extends RightActionQuiverFacade[AA, BB] {

        override lazy val chi: QUIVER[BB, TRUTH] = 
          new RightActionQuiver(target, omega, 
            RightIdeals.restrict(target.action.actionCarrier) {
                (t, m) => quiver.chi(target.action.actionMultiply(t, m)) 
            })        

        override def \[UU <: ELEMENT](monic: QUIVER[UU, BB]): QUIVER[AA, UU] = 
          monic.preBackDivide(this)

        override def preBackDivide[
          BBB <: Ɛ.ELEMENT, 
          Z <: Ɛ.ELEMENT, 
          ZZ <: ELEMENT
        ] (that: RightActionQuiver[Z, ZZ, BBB, BB]): QUIVER[ZZ, AA] = {
          val hackedThat = that.asInstanceOf[RightActionQuiver[Z, ZZ, B, BB]]
          new RightActionQuiver(hackedThat.source, source, hackedThat.quiver \ quiver)
        }

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[AA, BB]): EQUALIZER[AA] = 
          that.preEqualizer[A, B](this)

        override def preEqualizer[
          AAA <: Ɛ.ELEMENT, 
          BBB <: Ɛ.ELEMENT
        ] (that: RightActionQuiver[AAA, AA, BBB, BB]): EQUALIZER[AA] = {
          val thunkedEqualizer = quiver ?= that.asInstanceOf[RightActionQuiver[A, AA, B, BB]].quiver
          new RightActionStar[A, AA](
            rightAction(thunkedEqualizer)(source.action.actionMultiply(_, _)),
            source.↔
          ) with EqualizingStar[AA] { equalizingStar =>
            override val equalizerTarget = source
            override val inclusion: QUIVER[AA, AA] = new RightActionQuiver(equalizingStar, source, thunkedEqualizer.inclusion)
            override def restrict[RR <: ELEMENT](quiver: QUIVER[RR, AA]): QUIVER[RR, AA] =
              quiver.preRestrict[A](equalizingStar, thunkedEqualizer)
          }
        }

        override def preRestrict[BBB <: Ɛ.ELEMENT](
          equalizingStar: EQUALIZER[BB],
          thunkedEqualizer: Ɛ.EQUALIZER[BBB]
        ): QUIVER[AA, BB] =
          equalizingStar.crossPreRestrict[A, AA, B]( 
            source, 
            thunkedEqualizer.asInstanceOf[Ɛ.EQUALIZER[B]].restrict(quiver)
          )

        override def apply(a: AA): BB = target.↔ / quiver(source.↔ \ a)

        override def o[ZZ <: ELEMENT](that: QUIVER[ZZ, AA]): QUIVER[ZZ, BB] = 
          that.preCompose[A, B, BB](this)

        override def preCompose[
          BBB <: Ɛ.ELEMENT, 
          C <: Ɛ.ELEMENT, 
          CC <: ELEMENT
        ] (pre: RightActionQuiver[BBB, BB, C, CC]): QUIVER[AA, CC] = {
          val hackedPre = pre.asInstanceOf[RightActionQuiver[B, BB, C, CC]]
          new RightActionQuiver(source, hackedPre.target, hackedPre.quiver o quiver)
        }

        override def toString = "RightActionQuiver[" + quiver + "]"

        override def equals(other: Any): Boolean = other match {
          case that: RightActionQuiver[A, AA, B, BB] =>
            that.source == source &&
            that.target == target &&
            that.quiver == quiver
        }
        
        override def hashCode = 0
      }

      override type WRAPPER[T <: Ɛ.ELEMENT] = VanillaWrapper[T]

      private val memoizedStarWrapper = 
        Memoize.generic.withLowerBound[
          RightAction, 
          ({ type λ[T <: Ɛ.ELEMENT] = STAR[WRAPPER[T]]})#λ, 
          Ɛ.ELEMENT
        ](
          RightActionStar.wrap
        )

      override def star[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR[WRAPPER[T]] =
        memoizedStarWrapper(input) 

      override def quiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
        source: STAR[WRAPPER[S]], 
        target: STAR[WRAPPER[T]], 
        f: S => T
      ): QUIVER[WRAPPER[S], WRAPPER[T]] = {
        val src = source.asInstanceOf[RightActionStar[S, WRAPPER[S]]]
        val tgt = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        new RightActionQuiver[S, WRAPPER[S], T, WRAPPER[T]](
          src, 
          tgt, 
          src.action.actionCarrier(tgt.action.actionCarrier) { f }
        )}

      override def bifunctionAsBiQuiver[
        L <: Ɛ.ELEMENT, 
        R <: Ɛ.ELEMENT, 
        T <: Ɛ.ELEMENT
      ] (
        left: STAR[WRAPPER[L]],
        right: STAR[WRAPPER[R]],
        target: STAR[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]] = {
        val l =   left.asInstanceOf[RightActionStar[L, WRAPPER[L]]]
        val r =  right.asInstanceOf[RightActionStar[R, WRAPPER[R]]]
        val t = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        (l x r).biQuiver(t) { (a, b) =>
          VanillaWrapper(bifunc(a.element, b.element))
        }
      }
    }
  }
}


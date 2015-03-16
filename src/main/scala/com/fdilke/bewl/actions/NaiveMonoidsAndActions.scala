package com.fdilke.bewl.actions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.↔

// Monoids and monoid actions defined as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this
// Convention: for all these actions, the monoid acts on the right (makes the notation simpler)

trait NaiveMonoidsAndActions { 

  Ɛ: BaseTopos with AlgebraicMachinery with LogicalOperations =>

  case class NaiveMonoid[M <: ~](
    carrier: DOT[M], 
    unit: NullaryOp[M], 
    multiply: BinaryOp[M]
  ) { 

    monoid =>

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

    def action[
      A <: ~
    ] (
      actionCarrier: DOT[A]
    ) (
      actionMultiply: (A, M) => A
    ) =
      new Action[A](
        actionCarrier, 
        actionMultiply
      )

    lazy val regularAction = 
      action(carrier) { multiply(_, _) }

    case class Action[A <: ~] (
      actionCarrier: DOT[A],
      actionMultiply: (A, M) => A
    ) {
      def isMorphism[B <: ~](that: Action[B], quiver: ARROW[A, B]) =
        (quiver.source == this.actionCarrier) &&
        (quiver.target == that.actionCarrier) && (
          actionCarrier.forAll(carrier) { (x, m) =>
            that.actionCarrier.=?=(
              quiver(this.actionMultiply(x, m)),
              that.actionMultiply(quiver(x), m)
            )
          } toBool
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

    case class ActionPrequiver[
      S <: ~,
      T <: ~
    ] (
      source: Action[S],
      target: Action[T],
      function: S => T
    )

    lazy val actions = new Actions

    class Actions extends Topos with Wrappings[
      Ɛ.~,
      Action, 
      ActionPrequiver
    ] {
      trait ElementWrapper[
        A <: Ɛ.~
      ] {
        val element: A
      }

      object VanillaWrapper {
        def ↔[A <: Ɛ.~] = new ↔[A, VanillaWrapper[A]](
          a => VanillaWrapper(a),
          aa => aa.element
        )
        def apply[A <: Ɛ.~](a: A) =
          new VanillaWrapper(a)
      }

      class VanillaWrapper[
        A <: Ɛ.~
      ] (
        val element: A
      ) extends ElementWrapper[A] 

      class BiproductWrapper[
        A <: Ɛ.~,
        AA <: ~,
        B <: Ɛ.~,
        BB <: ~
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
        A <: Ɛ.~,
        AA <: ~,
        B <: Ɛ.~,
        BB <: ~
      ] (
        ma2b: Ɛ.>[Ɛ.x[M, A], B],
        aa2bb: AA => BB
      ) extends (AA => BB) with ElementWrapper[
        Ɛ.>[Ɛ.x[M, A], B]
      ] {
        def apply(aa: AA): BB = aa2bb(aa)

        override val element = ma2b
      }

      override type ~ = ElementWrapper[_ <: Ɛ.~]
      
      override type DOT[AA <: ~] = ActionDotFacade[AA]
      override type ARROW[AA <: ~, BB <: ~] = ActionArrowFacade[AA, BB]
      override type UNIT = VanillaWrapper[Ɛ.UNIT]

      // TODO: nothing's being overridden: simplify these. OR sort them out properly!
      override type x[SS <: ~, TT <: ~] = (SS, TT) with ~
      override type >[SS <: ~, TT <: ~] = (SS => TT) with ~

      type IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = VanillaWrapper[IDEAL]
      override val I: ActionDot[Ɛ.UNIT, UNIT] = ActionDot(Ɛ.I) { (i, m) => i }

      private object Ideals {
        private val possibleIdeals = carrier.power

        private val ideals = 
          possibleIdeals.forAll(carrier, carrier) {
            (f, m, n) => Ɛ.OmegaEnrichments(f(m)) > f(multiply(m, n))
          }.whereTrue

        def restrict[
          H <: Ɛ.~
        ] (
          that: Ɛ.DOT[H]
        ) (
          bifunc: (H, M) => Ɛ.TRUTH
        ): Ɛ.ARROW[H, IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = 
          restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }

        val omega = ActionDot[IDEAL](ideals)(
            Ɛ.BiQuiver[IDEAL, M, IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }

      override lazy val omega = Ideals.omega

      override lazy val truth: ARROW[UNIT, TRUTH] = 
        new ActionArrow(I, omega,
          Ideals.restrict(Ɛ.I) {
            (i, m) => Ɛ truth i
          })

      trait ActionDotFacade[
        AA <: ~
      ] extends Dot[AA] {
        def preMultiplyUncached[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionDot[Z, ZZ]
        ): BIPRODUCT[ZZ, AA]

        def preExponentiateUncached[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionDot[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA]

        def preApply[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionDot[Z, ZZ]
        )(
          f: ZZ => AA
        ): ARROW[ZZ, AA]

        def calcTranspose[
          R <: Ɛ.~,
          RR <: ~,
          T <: Ɛ.~,
          TT <: ~
        ] (
          source: ActionDot[R, RR],
          target: ActionDot[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialDot: ActionDot[
            Ɛ.>[Ɛ.x[M, R], T], 
            ExponentialWrapper[R, RR, T, TT]
          ],
          biQuiver: BiQuiver[AA, RR, TT]
        ): ARROW[AA, ExponentialWrapper[R, RR, T, TT]]

        def crossPreRestrict[
          Z <: Ɛ.~,
          ZZ <: ~,
          A <: Ɛ.~
        ] (
          source: ActionDot[Z, ZZ],
          restrictedArrow: Ɛ.ARROW[Z, A]
        ): ARROW[ZZ, AA]
      }

      class ActionDot[
        A <: Ɛ.~, 
        AA <: ~
      ] (
        val action: Action[A],
        val ↔ : A ↔ AA
      ) extends ActionDotFacade[AA] { star =>

        private lazy val pairs: Ɛ.BIPRODUCT[M, A] = 
          carrier x action.actionCarrier 

        override lazy val globals: Traversable[ARROW[UNIT, AA]] = { 
          val fixedPoints = action.actionCarrier.forAll(carrier) {
            (a, m) => action.actionCarrier.=?=(
              a, 
              action.actionMultiply(a, m)
            )
          }.whereTrue

          fixedPoints.globals.map { global =>
            new ActionArrow(I, star, fixedPoints.inclusion o global)
          }
        }

        override val toI: ARROW[AA, UNIT] = 
          new ActionArrow[A, AA, Ɛ.UNIT, UNIT](this, I, action.actionCarrier.toI) // TODO: need generics?

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[BB <: ~](
          that: DOT[BB]
        ) = that.preMultiplyUncached(this)

        override def preMultiplyUncached[Z <: Ɛ.~, ZZ <: ~](
          pre: ActionDot[Z, ZZ]
        ): BIPRODUCT[ZZ, AA] = {
          val product: Ɛ.BIPRODUCT[Z, A] = pre.action.actionCarrier x action.actionCarrier           
          new ActionDot[
              Ɛ.x[Z, A],
              BiproductWrapper[Z, ZZ, A, AA]
            ] (
              monoid.action(product){
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
            ) with BiproductDot[ZZ, AA, BiproductWrapper[Z, ZZ, A, AA]] {               
              override val left: DOT[ZZ] = pre
              override val right: DOT[AA] = star
              override def pair(zz: ZZ, aa: AA) = { 
                val z: Z = pre.↔ \ zz
                val a: A = star.↔ \ aa
                new BiproductWrapper[Z, ZZ, A, AA](zz, aa, product.pair(z, a))
            }}.asInstanceOf[BIPRODUCT[ZZ, AA]]
          }

        override def `>Uncached`[BB <: ~](that: DOT[BB]): EXPONENTIAL[AA, BB] =
          that.preExponentiateUncached(this)

        override def preExponentiateUncached[Z <: Ɛ.~, ZZ <: ~](
          pre: ActionDot[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA] = {
          val mXz = pre.pairs
          val possibleMorphisms = mXz > action.actionCarrier

          type P = Ɛ.>[Ɛ.x[M, Z],A]

          val morphisms: Ɛ.EQUALIZER[P] = 
            possibleMorphisms.forAll(carrier, carrier, pre.action.actionCarrier) {
              case (f, n, m, z) => 
                action.actionCarrier.=?=(
                  f(mXz.pair(multiply(m, n), pre.action.actionMultiply(z, n))),
                  action.actionMultiply(f(mXz.pair(m, z)), n)
                )
            }.whereTrue

          val morphismMultiply = morphisms.restrict(
            possibleMorphisms.transpose(
              (morphisms x carrier x mXz).biQuiver(action.actionCarrier) {
                case ((f, m), (n, z)) => morphisms.inclusion(f)(
                  mXz.pair(multiply(m, n), z)
              )}))

          new ActionDot[P, ExponentialWrapper[Z, ZZ, A, AA]](
            monoid.action(morphisms) {
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
            )) with ExponentialDot[
              ZZ, 
              AA, 
              ExponentialWrapper[Z, ZZ, A, AA]
            ] { exponentialDot =>
              val source: DOT[ZZ] = pre
              val target: DOT[AA] = star
              def transpose[RR <: ~](biQuiver: BiQuiver[RR, ZZ, AA]): ARROW[RR, ExponentialWrapper[Z, ZZ, A, AA]] =
                biQuiver.product.left.calcTranspose[Z, ZZ, A, AA](
                  pre, star, morphisms, possibleMorphisms, exponentialDot, biQuiver
                )
            }.asInstanceOf[EXPONENTIAL[ZZ, AA]]
        }

        override def calcTranspose[
          R <: Ɛ.~,
          RR <: ~,
          T <: Ɛ.~,
          TT <: ~
        ] (
          source: ActionDot[R, RR],
          target: ActionDot[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialDot: ActionDot[
            Ɛ.>[Ɛ.x[M, R], T], 
            ExponentialWrapper[R, RR, T, TT]
          ],
          biQuiver: BiQuiver[AA, RR, TT]
        ): ARROW[AA, ExponentialWrapper[R, RR, T, TT]] = {
          type P = Ɛ.>[Ɛ.x[M, R],T]
          val innerQuiver: Ɛ.ARROW[A, P] =
            morphisms.restrict(possibleMorphisms.transpose(
              (action.actionCarrier x source.pairs).biQuiver(target.action.actionCarrier) {
                case (a, (m, r)) => 
                  target.↔.\(biQuiver(
                    star.↔ / action.actionMultiply(a, m), 
                    source.↔ / r
                  ))}))
          this(exponentialDot) { aa =>
            exponentialDot.↔ / innerQuiver(↔ \ aa)
          }              
        }

        override def apply[
          BB <: ~
        ] (
          that: DOT[BB]
        ) (
          f: AA => BB
        ): ARROW[AA, BB] =
          that.preApply(this)(f)

        override def preApply[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionDot[Z, ZZ]
        ) (
          f: ZZ => AA
        ): ARROW[ZZ, AA] = 
          new ActionArrow(pre, star, 
            pre.action.actionCarrier(star.action.actionCarrier) { z =>
              star.↔ \ f(pre.↔ / z)
            })

        override def crossPreRestrict[
          Z <: Ɛ.~,
          ZZ <: ~,
          AAA <: Ɛ.~
        ] (
          source: ActionDot[Z, ZZ],
          restrictedQuiver: Ɛ.ARROW[Z, AAA]
        ): ARROW[ZZ, AA] = 
          new ActionArrow(
            source, 
            star,
            restrictedQuiver.asInstanceOf[Ɛ.ARROW[Z, A]]
          )

        override def toString = "ActionDot[" + action.actionCarrier + "]"
      }

      object ActionDot {
        def apply[
          A <: Ɛ.~
        ] (
          actionCarrier: Ɛ.DOT[A]
        ) (
          actionMultiply: (A, M) => A
        ) =
          wrap(monoid.action(actionCarrier)(actionMultiply))

        def wrap[
          A <: Ɛ.~
        ] (
          action: Action[A]
        ) =
          new ActionDot[
            A, 
            VanillaWrapper[A]
          ](
            action,
            VanillaWrapper.↔[A]
          ) 
      }

      trait ActionArrowFacade[
        AA <: ~,
        BB <: ~
      ] extends Arrow[AA, BB] { 

        def preBackDivide[
          B <: Ɛ.~,
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionArrow[Z, ZZ, B, BB]
        ): ARROW[ZZ, AA]

        def preEqualizer[
          A <: Ɛ.~,
          B <: Ɛ.~
        ] (pre: ActionArrow[A, AA, B, BB]): EQUALIZER[AA]

        def preRestrict[
          B <: Ɛ.~
        ] (
          equalizingDot: EQUALIZER[BB],
          thunkedEqualizer: Ɛ.EQUALIZER[B]
        ): ARROW[AA, BB]

        def preCompose[
          B <: Ɛ.~,
          C <: Ɛ.~,
          CC <: ~
        ] (
          pre: ActionArrow[B, BB, C, CC]
        ): ARROW[AA, CC]
      }

      class ActionArrow[
        A <: Ɛ.~,
        AA <: ~,
        B <: Ɛ.~,
        BB <: ~
      ] (
        val source: ActionDot[A, AA],
        val target: ActionDot[B, BB],
        val quiver: Ɛ.ARROW[A, B]
      ) extends ActionArrowFacade[AA, BB] {

        override lazy val chi: ARROW[BB, TRUTH] = 
          new ActionArrow(target, omega, 
            Ideals.restrict(target.action.actionCarrier) {
                (t, m) => quiver.chi(target.action.actionMultiply(t, m)) 
            })        

        override def \[UU <: ~](monic: ARROW[UU, BB]): ARROW[AA, UU] =
          monic.preBackDivide(this)

        override def preBackDivide[
          BBB <: Ɛ.~,
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          that: ActionArrow[Z, ZZ, BBB, BB]
        ): ARROW[ZZ, AA] = {
          val hackedThat = that.asInstanceOf[ActionArrow[Z, ZZ, B, BB]]
          new ActionArrow(hackedThat.source, source, hackedThat.quiver \ quiver)
        }

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: ARROW[AA, BB]): EQUALIZER[AA] = 
          that.preEqualizer[A, B](this)

        override def preEqualizer[
          AAA <: Ɛ.~,
          BBB <: Ɛ.~
        ] (
          that: ActionArrow[AAA, AA, BBB, BB]
        ): EQUALIZER[AA] = {
          val thunkedEqualizer = quiver ?= that.asInstanceOf[ActionArrow[A, AA, B, BB]].quiver
          new ActionDot[A, AA](
            monoid.action(thunkedEqualizer)(source.action.actionMultiply(_, _)),
            source.↔
          ) with EqualizingDot[AA] { equalizingDot =>
            override val equalizerTarget = source
            override val inclusion: ARROW[AA, AA] = new ActionArrow(equalizingDot, source, thunkedEqualizer.inclusion)
            override def restrict[RR <: ~](quiver: ARROW[RR, AA]): ARROW[RR, AA] =
              quiver.preRestrict[A](equalizingDot, thunkedEqualizer)
          }
        }

        override def preRestrict[BBB <: Ɛ.~](
          equalizingDot: EQUALIZER[BB],
          thunkedEqualizer: Ɛ.EQUALIZER[BBB]
        ): ARROW[AA, BB] =
          equalizingDot.crossPreRestrict[A, AA, B]( 
            source, 
            thunkedEqualizer.asInstanceOf[Ɛ.EQUALIZER[B]].restrict(quiver)
          )

        override def apply(a: AA): BB = target.↔ / quiver(source.↔ \ a)

        override def o[ZZ <: ~](that: ARROW[ZZ, AA]): ARROW[ZZ, BB] =
          that.preCompose[A, B, BB](this)

        override def preCompose[
          BBB <: Ɛ.~,
          C <: Ɛ.~,
          CC <: ~
        ] (pre: ActionArrow[BBB, BB, C, CC]): ARROW[AA, CC] = {
          val hackedPre = pre.asInstanceOf[ActionArrow[B, BB, C, CC]]
          new ActionArrow(source, hackedPre.target, hackedPre.quiver o quiver)
        }

        override def toString = "ActionArrow[" + quiver + "]"

        override def equals(other: Any): Boolean = other match {
          case that: ActionArrow[A, AA, B, BB] =>
            that.source == source &&
            that.target == target &&
            that.quiver == quiver
        }
        
        override def hashCode = 0
      }

      override type WRAPPER[
        T <: Ɛ.~
      ] = VanillaWrapper[T]

      private val memoizedStarWrapper = 
        Memoize.generic withLowerBound[
          Action, 
          ({ type λ[T <: Ɛ.~] = DOT[WRAPPER[T]]})#λ,
          Ɛ.~
        ] ActionDot.wrap
        

      override def star[
        T <: Ɛ.~
      ] (input: Action[T]): DOT[WRAPPER[T]] =
        memoizedStarWrapper(input) 

      override def quiver[
        S <: Ɛ.~,
        T <: Ɛ.~
      ] (
        prequiver: ActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[
        S <: Ɛ.~,
        T <: Ɛ.~
      ] (
        source: DOT[WRAPPER[S]], 
        target: DOT[WRAPPER[T]], 
        f: S => T
      ): ARROW[WRAPPER[S], WRAPPER[T]] = {
        val src = source.asInstanceOf[ActionDot[S, WRAPPER[S]]]
        val tgt = target.asInstanceOf[ActionDot[T, WRAPPER[T]]]
        new ActionArrow[S, WRAPPER[S], T, WRAPPER[T]](
          src, 
          tgt, 
          src.action.actionCarrier(tgt.action.actionCarrier) { f }
        )}

      override def bifunctionAsBiQuiver[
        L <: Ɛ.~,
        R <: Ɛ.~,
        T <: Ɛ.~
      ] (
        left: DOT[WRAPPER[L]],
        right: DOT[WRAPPER[R]],
        target: DOT[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]] = {
        val l =   left.asInstanceOf[ActionDot[L, WRAPPER[L]]]
        val r =  right.asInstanceOf[ActionDot[R, WRAPPER[R]]]
        val t = target.asInstanceOf[ActionDot[T, WRAPPER[T]]]
        (l x r).biQuiver(t) { (a, b) =>
          VanillaWrapper(bifunc(a.element, b.element))
        }
      }
    }
  }
}


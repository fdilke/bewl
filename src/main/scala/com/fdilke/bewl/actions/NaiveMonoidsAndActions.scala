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
      def isMorphism[B <: ~](that: Action[B], quiver: QUIVER[A, B]) =
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
      
      override type DOT[AA <: ~] = ActionStarFacade[AA]
      override type QUIVER[AA <: ~, BB <: ~] = ActionQuiverFacade[AA, BB]
      override type UNIT = VanillaWrapper[Ɛ.UNIT]

      // TODO: nothing's being overridden: simplify these. OR sort them out properly!
      override type x[SS <: ~, TT <: ~] = (SS, TT) with ~
      override type >[SS <: ~, TT <: ~] = (SS => TT) with ~

      type IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = VanillaWrapper[IDEAL]
      override val I: ActionStar[Ɛ.UNIT, UNIT] = ActionStar(Ɛ.I) { (i, m) => i }

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
        ): Ɛ.QUIVER[H, IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = 
          restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }

        val omega = ActionStar[IDEAL](ideals)(
            Ɛ.BiQuiver[IDEAL, M, IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }

      override lazy val omega = Ideals.omega

      override lazy val truth: QUIVER[UNIT, TRUTH] = 
        new ActionQuiver(I, omega,
          Ideals.restrict(Ɛ.I) {
            (i, m) => Ɛ truth i
          })

      trait ActionStarFacade[
        AA <: ~
      ] extends Star[AA] {
        def preMultiplyUncached[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionStar[Z, ZZ]
        ): BIPRODUCT[ZZ, AA]

        def preExponentiateUncached[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionStar[Z, ZZ]
        ): EXPONENTIAL[ZZ, AA]

        def preApply[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionStar[Z, ZZ]
        )(
          f: ZZ => AA
        ): QUIVER[ZZ, AA]

        def calcTranspose[
          R <: Ɛ.~,
          RR <: ~,
          T <: Ɛ.~,
          TT <: ~
        ] (
          source: ActionStar[R, RR],
          target: ActionStar[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialStar: ActionStar[
            Ɛ.>[Ɛ.x[M, R], T], 
            ExponentialWrapper[R, RR, T, TT]
          ],
          biQuiver: BiQuiver[AA, RR, TT]
        ): QUIVER[AA, ExponentialWrapper[R, RR, T, TT]]

        def crossPreRestrict[
          Z <: Ɛ.~,
          ZZ <: ~,
          A <: Ɛ.~
        ] (
          source: ActionStar[Z, ZZ],
          restrictedQuiver: Ɛ.QUIVER[Z, A]
        ): QUIVER[ZZ, AA]
      }

      class ActionStar[
        A <: Ɛ.~, 
        AA <: ~
      ] (
        val action: Action[A],
        val ↔ : A ↔ AA
      ) extends ActionStarFacade[AA] { star =>

        private lazy val pairs: Ɛ.BIPRODUCT[M, A] = 
          carrier x action.actionCarrier 

        override lazy val globals: Traversable[QUIVER[UNIT, AA]] = { 
          val fixedPoints = action.actionCarrier.forAll(carrier) {
            (a, m) => action.actionCarrier.=?=(
              a, 
              action.actionMultiply(a, m)
            )
          }.whereTrue

          fixedPoints.globals.map { global =>
            new ActionQuiver(I, star, fixedPoints.inclusion o global)
          }
        }

        override val toI: QUIVER[AA, UNIT] = 
          new ActionQuiver[A, AA, Ɛ.UNIT, UNIT](this, I, action.actionCarrier.toI) // TODO: need generics?

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[BB <: ~](
          that: DOT[BB]
        ) = that.preMultiplyUncached(this)

        override def preMultiplyUncached[Z <: Ɛ.~, ZZ <: ~](
          pre: ActionStar[Z, ZZ]
        ): BIPRODUCT[ZZ, AA] = {
          val product: Ɛ.BIPRODUCT[Z, A] = pre.action.actionCarrier x action.actionCarrier           
          new ActionStar[
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
            ) with BiproductStar[ZZ, AA, BiproductWrapper[Z, ZZ, A, AA]] {               
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
          pre: ActionStar[Z, ZZ]
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

          new ActionStar[P, ExponentialWrapper[Z, ZZ, A, AA]](
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
            )) with ExponentialStar[
              ZZ, 
              AA, 
              ExponentialWrapper[Z, ZZ, A, AA]
            ] { exponentialStar =>
              val source: DOT[ZZ] = pre
              val target: DOT[AA] = star
              def transpose[RR <: ~](biQuiver: BiQuiver[RR, ZZ, AA]): QUIVER[RR, ExponentialWrapper[Z, ZZ, A, AA]] =
                biQuiver.product.left.calcTranspose[Z, ZZ, A, AA](
                  pre, star, morphisms, possibleMorphisms, exponentialStar, biQuiver
                )
            }.asInstanceOf[EXPONENTIAL[ZZ, AA]]
        }

        override def calcTranspose[
          R <: Ɛ.~,
          RR <: ~,
          T <: Ɛ.~,
          TT <: ~
        ] (
          source: ActionStar[R, RR],
          target: ActionStar[T, TT],
          morphisms: Ɛ.EQUALIZER[Ɛ.>[Ɛ.x[M, R], T]],
          possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
          exponentialStar: ActionStar[
            Ɛ.>[Ɛ.x[M, R], T], 
            ExponentialWrapper[R, RR, T, TT]
          ],
          biQuiver: BiQuiver[AA, RR, TT]
        ): QUIVER[AA, ExponentialWrapper[R, RR, T, TT]] = {
          type P = Ɛ.>[Ɛ.x[M, R],T]
          val innerQuiver: Ɛ.QUIVER[A, P] =
            morphisms.restrict(possibleMorphisms.transpose(
              (action.actionCarrier x source.pairs).biQuiver(target.action.actionCarrier) {
                case (a, (m, r)) => 
                  target.↔.\(biQuiver(
                    star.↔ / action.actionMultiply(a, m), 
                    source.↔ / r
                  ))}))
          this(exponentialStar) { aa =>
            exponentialStar.↔ / innerQuiver(↔ \ aa)
          }              
        }

        override def apply[
          BB <: ~
        ] (
          that: DOT[BB]
        ) (
          f: AA => BB
        ): QUIVER[AA, BB] =
          that.preApply(this)(f)

        override def preApply[
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionStar[Z, ZZ]
        ) (
          f: ZZ => AA
        ): QUIVER[ZZ, AA] = 
          new ActionQuiver(pre, star, 
            pre.action.actionCarrier(star.action.actionCarrier) { z =>
              star.↔ \ f(pre.↔ / z)
            })

        override def crossPreRestrict[
          Z <: Ɛ.~,
          ZZ <: ~,
          AAA <: Ɛ.~
        ] (
          source: ActionStar[Z, ZZ],
          restrictedQuiver: Ɛ.QUIVER[Z, AAA]
        ): QUIVER[ZZ, AA] = 
          new ActionQuiver(
            source, 
            star,
            restrictedQuiver.asInstanceOf[Ɛ.QUIVER[Z, A]]
          )

        override def toString = "ActionStar[" + action.actionCarrier + "]"
      }

      object ActionStar {
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
          new ActionStar[
            A, 
            VanillaWrapper[A]
          ](
            action,
            VanillaWrapper.↔[A]
          ) 
      }

      trait ActionQuiverFacade[
        AA <: ~,
        BB <: ~
      ] extends Quiver[AA, BB] { 

        def preBackDivide[
          B <: Ɛ.~,
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          pre: ActionQuiver[Z, ZZ, B, BB]
        ): QUIVER[ZZ, AA]

        def preEqualizer[
          A <: Ɛ.~,
          B <: Ɛ.~
        ] (pre: ActionQuiver[A, AA, B, BB]): EQUALIZER[AA]

        def preRestrict[
          B <: Ɛ.~
        ] (
          equalizingStar: EQUALIZER[BB],
          thunkedEqualizer: Ɛ.EQUALIZER[B]
        ): QUIVER[AA, BB]

        def preCompose[
          B <: Ɛ.~,
          C <: Ɛ.~,
          CC <: ~
        ] (
          pre: ActionQuiver[B, BB, C, CC]
        ): QUIVER[AA, CC]
      }

      class ActionQuiver[
        A <: Ɛ.~,
        AA <: ~,
        B <: Ɛ.~,
        BB <: ~
      ] (
        val source: ActionStar[A, AA],
        val target: ActionStar[B, BB],
        val quiver: Ɛ.QUIVER[A, B]
      ) extends ActionQuiverFacade[AA, BB] {

        override lazy val chi: QUIVER[BB, TRUTH] = 
          new ActionQuiver(target, omega, 
            Ideals.restrict(target.action.actionCarrier) {
                (t, m) => quiver.chi(target.action.actionMultiply(t, m)) 
            })        

        override def \[UU <: ~](monic: QUIVER[UU, BB]): QUIVER[AA, UU] =
          monic.preBackDivide(this)

        override def preBackDivide[
          BBB <: Ɛ.~,
          Z <: Ɛ.~,
          ZZ <: ~
        ] (
          that: ActionQuiver[Z, ZZ, BBB, BB]
        ): QUIVER[ZZ, AA] = {
          val hackedThat = that.asInstanceOf[ActionQuiver[Z, ZZ, B, BB]]
          new ActionQuiver(hackedThat.source, source, hackedThat.quiver \ quiver)
        }

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[AA, BB]): EQUALIZER[AA] = 
          that.preEqualizer[A, B](this)

        override def preEqualizer[
          AAA <: Ɛ.~,
          BBB <: Ɛ.~
        ] (
          that: ActionQuiver[AAA, AA, BBB, BB]
        ): EQUALIZER[AA] = {
          val thunkedEqualizer = quiver ?= that.asInstanceOf[ActionQuiver[A, AA, B, BB]].quiver
          new ActionStar[A, AA](
            monoid.action(thunkedEqualizer)(source.action.actionMultiply(_, _)),
            source.↔
          ) with EqualizingStar[AA] { equalizingStar =>
            override val equalizerTarget = source
            override val inclusion: QUIVER[AA, AA] = new ActionQuiver(equalizingStar, source, thunkedEqualizer.inclusion)
            override def restrict[RR <: ~](quiver: QUIVER[RR, AA]): QUIVER[RR, AA] =
              quiver.preRestrict[A](equalizingStar, thunkedEqualizer)
          }
        }

        override def preRestrict[BBB <: Ɛ.~](
          equalizingStar: EQUALIZER[BB],
          thunkedEqualizer: Ɛ.EQUALIZER[BBB]
        ): QUIVER[AA, BB] =
          equalizingStar.crossPreRestrict[A, AA, B]( 
            source, 
            thunkedEqualizer.asInstanceOf[Ɛ.EQUALIZER[B]].restrict(quiver)
          )

        override def apply(a: AA): BB = target.↔ / quiver(source.↔ \ a)

        override def o[ZZ <: ~](that: QUIVER[ZZ, AA]): QUIVER[ZZ, BB] =
          that.preCompose[A, B, BB](this)

        override def preCompose[
          BBB <: Ɛ.~,
          C <: Ɛ.~,
          CC <: ~
        ] (pre: ActionQuiver[BBB, BB, C, CC]): QUIVER[AA, CC] = {
          val hackedPre = pre.asInstanceOf[ActionQuiver[B, BB, C, CC]]
          new ActionQuiver(source, hackedPre.target, hackedPre.quiver o quiver)
        }

        override def toString = "ActionQuiver[" + quiver + "]"

        override def equals(other: Any): Boolean = other match {
          case that: ActionQuiver[A, AA, B, BB] =>
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
        ] ActionStar.wrap
        

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
      ): QUIVER[WRAPPER[S], WRAPPER[T]] = {
        val src = source.asInstanceOf[ActionStar[S, WRAPPER[S]]]
        val tgt = target.asInstanceOf[ActionStar[T, WRAPPER[T]]]
        new ActionQuiver[S, WRAPPER[S], T, WRAPPER[T]](
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
        val l =   left.asInstanceOf[ActionStar[L, WRAPPER[L]]]
        val r =  right.asInstanceOf[ActionStar[R, WRAPPER[R]]]
        val t = target.asInstanceOf[ActionStar[T, WRAPPER[T]]]
        (l x r).biQuiver(t) { (a, b) =>
          VanillaWrapper(bifunc(a.element, b.element))
        }
      }
    }
  }
}


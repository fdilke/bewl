package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.↔
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import com.fdilke.bewl.helper.{Memoize, ⊕}

trait ConstructToposOfMonoidActions extends
  BaseTopos with
  ToposEnrichments {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfMonoidActions {
    type ~~ =
      ElementWrapper[_ <: ~]

    def of[
      M <: ~
    ] (
      monoid: Ɛ.Monoid[M]
    ) : Topos[~~] with
      Wrappings[
        ~~,
        ~,
        ({type λ[X <: ~] = monoid.Action[X]}) # λ,
        ({type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y]}) # λ,
        ({type λ[X <: ~] = Ɛ.VanillaWrapper[X]}) # λ
      ] =
        new Topos[~~] with Wrappings[
          ~~,
          ~,
          ({type λ[X <: ~] = monoid.Action[X]}) # λ,
          ({type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y]}) # λ,
          ({type λ[X <: ~] = Ɛ.VanillaWrapper[X]}) # λ
        ] {
          import monoid.{carrier, multiply, unit, Action, ActionPreArrow}

          class BiproductWrapper[
            A <: Ɛ.~,
            AA <: ~~,
            B <: Ɛ.~,
            BB <: ~~
          ](
            aa: AA,
            bb: BB,
            aXb: Ɛ.x[A, B]
          ) extends ⊕[AA, BB](aa, bb) with Ɛ.ElementWrapper[
            Ɛ.x[A, B]
          ] {
            override val element = aXb
          }

          class ExponentialWrapper[
            A <: Ɛ.~,
            AA <: ~~,
            B <: Ɛ.~,
            BB <: ~~
          ](
            ma2b: Ɛ.→[Ɛ.x[M, A], B],
            aa2bb: AA => BB
          ) extends (AA => BB) with Ɛ.ElementWrapper[
            Ɛ.→[Ɛ.x[M, A], B]
          ] {
            def apply(aa: AA): BB = aa2bb(aa)
            override val element = ma2b
          }

          override type DOT[AA <: ~~] = ActionDotFacade[AA]
          override type >[AA <: ~~, BB <: ~~] = ActionArrowFacade[AA, BB]
          override type UNIT = Ɛ.VanillaWrapper[Ɛ.UNIT]
          override type →[T <: ~, U <: ~] = ~

          type IDEAL = Ɛ.→[M, Ɛ.TRUTH]

          override type TRUTH = Ɛ.VanillaWrapper[IDEAL]
          override val I: ActionDot[Ɛ.UNIT, UNIT] =
            ActionDot(Ɛ.I) {
              (i, m) => i
            }

          private object Ideals {
            private val possibleIdeals = carrier.power
            import possibleIdeals.{ evaluate => $ }

            private val ideals =
              possibleIdeals.whereAll(carrier, carrier) {
                (f, m, n) => Ɛ.OmegaEnrichments($(f, m)) → $(f, multiply(m, n))
              }

            def restrict[
              H <: Ɛ.~
            ](
              that: Ɛ.DOT[H]
            )(
              bifunc: (H, M) => Ɛ.TRUTH
            ): Ɛ.>[H, IDEAL] =
              ideals.restrict(possibleIdeals.transpose(that)(bifunc))

            private val idealMultiply =
              restrict(ideals x carrier) {
                case (i ⊕ s, t) => $(ideals.inclusion(i), multiply(s, t))
              }

            val omega = ActionDot[IDEAL](ideals)(
              Ɛ.BiArrow[IDEAL, M, IDEAL](ideals x carrier, idealMultiply)(_, _)
            )
          }

          override lazy val omega = Ideals.omega

          override lazy val truth: UNIT > TRUTH = // TODO: don't need generics in next line
            ActionArrow[Ɛ.UNIT, UNIT, IDEAL, Ɛ.VanillaWrapper[IDEAL]](
              I,
              omega,
              Ideals.restrict(Ɛ.I) {
                (i, m) => Ɛ truth i
              }
            )

          trait ActionDotFacade[
            AA <: ~~
          ] extends Dot[AA] {
            def preMultiplyUncached[
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              pre: ActionDot[Z, ZZ]
            ): BIPRODUCT[ZZ, AA]

            def preExponentiateUncached[
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              pre: ActionDot[Z, ZZ]
            ): EXPONENTIAL[ZZ, AA]

            def preApply[
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              pre: ActionDot[Z, ZZ]
            )(
              f: ZZ => AA
            ): ZZ > AA

            def calcTranspose[
              R <: Ɛ.~,
              RR <: ~~,
              T <: Ɛ.~,
              TT <: ~~
            ](
              source: ActionDot[R, RR],
              target: ActionDot[T, TT],
              morphisms: Ɛ.EQUALIZER[Ɛ.→[Ɛ.x[M, R], T]],
              possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
              exponentialDot: ActionDot[
                Ɛ.→[Ɛ.x[M, R], T],
                ExponentialWrapper[R, RR, T, TT]
              ],
              biArrow: BiArrow[AA, RR, TT]
            ): AA > ExponentialWrapper[R, RR, T, TT]

            def crossPreRestrict[
              Z <: Ɛ.~,
              ZZ <: ~~,
              A <: Ɛ.~
            ](
              source: ActionDot[Z, ZZ],
              restrictedArrow: Ɛ.>[Z, A]
            ): ZZ > AA
          }

          class ActionDot[
            A <: Ɛ.~,
            AA <: ~~
          ](
            val action: Action[A],
            val ↔ : A ↔ AA
          ) extends ActionDotFacade[AA] { dot =>

            private lazy val pairs: Ɛ.BIPRODUCT[M, A] =
              carrier x action.actionCarrier

            override lazy val globals: Traversable[UNIT > AA] = {
              val fixedPoints =
                action.actionCarrier.whereAll(
                    carrier
                ) {
                  (a, m) => 
                    action.actionCarrier.=?=(
                      a,
                      action.actionMultiply(a, m)
                    )
                }

              fixedPoints.globals.map { global =>
                ActionArrow(
                  I, 
                  dot, 
                  fixedPoints.inclusion o global
                )
              }
            }

            override val toI: AA > UNIT =
              ActionArrow(this, I, action.actionCarrier.toI)

            override def sanityTest = {
              action.actionCarrier.sanityTest
              action.sanityTest
            }

            override def xUncached[
              BB <: ~~
            ] (
              that: DOT[BB]
            ) =
              that preMultiplyUncached this

            override def preMultiplyUncached[
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              pre: ActionDot[Z, ZZ]
            ): BIPRODUCT[ZZ, AA] = {
              val product: Ɛ.BIPRODUCT[Z, A] =
                pre.action.actionCarrier x action.actionCarrier

              new ActionDot[
                Ɛ.x[Z, A],
                BiproductWrapper[Z, ZZ, A, AA]
              ](
                monoid.action(product) {
                  case (z ⊕ a, m) => product.pair(
                    pre.action.actionMultiply(z, m),
                    action.actionMultiply(a, m)
                  )
                },
                new ↔[Ɛ.x[Z, A], BiproductWrapper[Z, ZZ, A, AA]](
                  zxa => zxa match {
                    case z ⊕ a =>
                      val zz: ZZ = pre.↔ / z
                      val aa: AA = dot.↔ / a
                      new BiproductWrapper[Z, ZZ, A, AA](zz, aa, zxa)
                  },
                  zzXaa => zzXaa.element
                )
              ) with BiproductDot[ZZ, AA, BiproductWrapper[Z, ZZ, A, AA]] {
                override val left: DOT[ZZ] = pre
                override val right: DOT[AA] = dot

                override def pair(zz: ZZ, aa: AA) = {
                  val z: Z = pre.↔ \ zz
                  val a: A = dot.↔ \ aa
                  new BiproductWrapper[Z, ZZ, A, AA](zz, aa, product.pair(z, a))
                }
              }.asInstanceOf[BIPRODUCT[ZZ, AA]]
            }

            override def `>Uncached`[BB <: ~~](that: DOT[BB]): EXPONENTIAL[AA, BB] =
              that.preExponentiateUncached(this)

            override def preExponentiateUncached[Z <: Ɛ.~, ZZ <: ~~](
              pre: ActionDot[Z, ZZ]
            ): EXPONENTIAL[ZZ, AA] = {
              val mXz = pre.pairs
              val possibleMorphisms = mXz > action.actionCarrier
              import possibleMorphisms.{ evaluate => $ }

              type P = Ɛ.→[Ɛ.x[M, Z], A]

              val morphisms: Ɛ.EQUALIZER[P] =
                possibleMorphisms.whereAll(
                  carrier,
                  carrier,
                  pre.action.actionCarrier
                ) {
                  case (f, n, m, z) =>
                    action.actionCarrier.=?=(
                      $(f, 
                        mXz.pair(
                          multiply(m, n),
                          pre.action.actionMultiply(z, n)
                        )
                      ),
                      action.actionMultiply(
                        $(f, 
                          mXz.pair(m, z)
                        ),
                        n
                      )
                    )
                }

              val morphismMultiply =
                morphisms.restrict(
                  possibleMorphisms.transpose(
                    morphisms x carrier
                  ) {
                    case (f ⊕ m, n ⊕ z) =>
                      $(morphisms.inclusion(f),
                        mXz.pair(multiply(m, n), z)
                      )
                  }
                )

              new ActionDot[P, ExponentialWrapper[Z, ZZ, A, AA]](
                monoid.action(morphisms) {
                  Ɛ.BiArrow(morphisms x carrier, morphismMultiply)(_, _)
                },
                new ↔[P, ExponentialWrapper[Z, ZZ, A, AA]](
                  p => new ExponentialWrapper[Z, ZZ, A, AA](p,
                    (zz: ZZ) => {
                      val z = pre.↔ \ zz
                      val unitM: M = unit(pre.action.actionCarrier.toI(z))
                      val a: A = $(p, mXz.pair(unitM, z))
                      dot.↔ / a
                    }
                  ),
                  zz2aa => zz2aa.element
                )) with ExponentialDot[
                  ZZ,
                  AA,
                  ExponentialWrapper[Z, ZZ, A, AA]
                ] {
                  exponentialDot =>
                  override val source: DOT[ZZ] = pre
                  override val target: DOT[AA] = dot

                  override def evaluate(
                      function: ExponentialWrapper[Z, ZZ, A, AA], 
                      arg: ZZ
                  ): AA =
                    function(arg)
                  
                  override def transpose[RR <: ~~](
                    biArrow: BiArrow[RR, ZZ, AA]
                  ): RR > ExponentialWrapper[Z, ZZ, A, AA] =
                    biArrow.product.left.calcTranspose[Z, ZZ, A, AA](
                      pre, dot, morphisms, possibleMorphisms, exponentialDot, biArrow
                    )
              }.asInstanceOf[EXPONENTIAL[ZZ, AA]]
            }

            override def calcTranspose[
              R <: Ɛ.~,
              RR <: ~~,
              T <: Ɛ.~,
              TT <: ~~
            ](
              source: ActionDot[R, RR],
              target: ActionDot[T, TT],
              morphisms: Ɛ.EQUALIZER[Ɛ.→[Ɛ.x[M, R], T]],
              possibleMorphisms: Ɛ.EXPONENTIAL[Ɛ.x[M, R], T],
              exponentialDot: ActionDot[
                Ɛ.→[Ɛ.x[M, R], T],
                ExponentialWrapper[R, RR, T, TT]
              ],
              biArrow: BiArrow[AA, RR, TT]
            ): AA > ExponentialWrapper[R, RR, T, TT] = {
              type P = Ɛ.→[Ɛ.x[M, R], T]
              val innerArrow: Ɛ.>[A, P] =
                morphisms.restrict(
                  possibleMorphisms.transpose(action.actionCarrier) {
                    case (a, m ⊕ r) =>
                      target.↔.\(biArrow(
                        dot.↔ / action.actionMultiply(a, m),
                        source.↔ / r
                      ))
                  }
                )
              this(exponentialDot) { aa =>
                exponentialDot.↔ / innerArrow(↔ \ aa)
              }
            }

            override def apply[
              BB <: ~~
            ](
              that: DOT[BB]
            )(
              f: AA => BB
            ): AA > BB =
              that.preApply(this)(f)

            override def preApply[
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              pre: ActionDot[Z, ZZ]
            )(
              f: ZZ => AA
            ): ZZ > AA =
              ActionArrow(pre, dot,
                pre.action.actionCarrier(dot.action.actionCarrier) { z =>
                  dot.↔ \ f(pre.↔ / z)
                })

            override def crossPreRestrict[
              Z <: Ɛ.~,
              ZZ <: ~~,
              AAA <: Ɛ.~
            ](
              source: ActionDot[Z, ZZ],
              restrictedArrow: Ɛ.>[Z, AAA]
            ): ZZ > AA =
              ActionArrow(
                source,
                dot,
                restrictedArrow.asInstanceOf[Ɛ.>[Z, A]]
              )

            override def toString =
              "ActionDot[" + action.actionCarrier + "]"
          }

          object ActionDot {
            def apply[
              A <: Ɛ.~
            ](
              actionCarrier: Ɛ.DOT[A]
            )(
              actionMultiply: (A, M) => A
            ): ActionDot[
              A,
              Ɛ.VanillaWrapper[A]
            ] =
              wrap(monoid.action(actionCarrier)(actionMultiply))

            def wrap[
              A <: Ɛ.~
            ](
              action: Action[A]
            ) =
              new ActionDot[
                A,
                Ɛ.VanillaWrapper[A]
              ](
                action,
                Ɛ.VanillaWrapper.↔[A]
              )
          }

          trait ActionArrowFacade[
            AA <: ~~,
            BB <: ~~
          ] extends Arrow[AA, BB] {

            def preBackDivide[
              B <: Ɛ.~,
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              pre: ActionArrow[Z, ZZ, B, BB]
            ): ZZ > AA

            def preEqualizer[
              A <: Ɛ.~,
              B <: Ɛ.~
            ] (
              pre: ActionArrow[A, AA, B, BB]
            ): EQUALIZER[AA]

            def preRestrict[
              B <: Ɛ.~
            ](
              equalizingDot: EQUALIZER[BB],
              thunkedEqualizer: Ɛ.EQUALIZER[B]
            ): AA > BB

            def preCompose[
              B <: Ɛ.~,
              C <: Ɛ.~,
              CC <: ~~
            ](
              pre: ActionArrow[B, BB, C, CC]
            ): AA > CC
          }

          case class ActionArrow[
            A <: Ɛ.~,
            AA <: ~~,
            B <: Ɛ.~,
            BB <: ~~
          ](
            source: ActionDot[A, AA],
            target: ActionDot[B, BB],
            arrow: Ɛ.>[A, B]
          ) extends ActionArrowFacade[AA, BB] {

            override lazy val chi: BB > TRUTH =
              ActionArrow(
                target,
                omega,
                Ideals.restrict(target.action.actionCarrier) {
                  (t, m) => arrow.chi(target.action.actionMultiply(t, m))
                })

            override def \[
              UU <: ~~
            ] (
              monic: UU > BB
            ): AA > UU =
              monic preBackDivide this

            override def preBackDivide[
              BBB <: Ɛ.~,
              Z <: Ɛ.~,
              ZZ <: ~~
            ](
              that: ActionArrow[Z, ZZ, BBB, BB]
            ): ZZ > AA = {
              val hackedThat =
                that.asInstanceOf[
                  ActionArrow[Z, ZZ, B, BB]
                ]
              ActionArrow(hackedThat.source, source, hackedThat.arrow \ arrow)
            }

            override def sanityTest = {
              arrow.sanityTest
              assert(monoid.actions.isMorphism(source.action, target.action, arrow))
            }

            override def ?=(that: AA > BB): EQUALIZER[AA] =
              that.preEqualizer[A, B](this)

            override def preEqualizer[
              AAA <: Ɛ.~,
              BBB <: Ɛ.~
            ](
             that: ActionArrow[AAA, AA, BBB, BB]
            ): EQUALIZER[AA] = {
              val thunkedEqualizer = arrow ?= that.asInstanceOf[ActionArrow[A, AA, B, BB]].arrow
              new ActionDot[A, AA](
                monoid.action(
                  thunkedEqualizer
                ) (
                  source.action.actionMultiply
                ),
                source.↔
              ) with EqualizingDot[AA] { equalizingDot =>
                override val equalizerTarget =
                  source

                override def restrict[
                  RR <: ~~
                ] (
                  arrow: RR > AA
                ): RR > AA =
                  arrow.preRestrict[A](
                    equalizingDot,
                    thunkedEqualizer
                  )
              }
            }

            override def preRestrict[
              BBB <: Ɛ.~
            ] (
              equalizingDot: EQUALIZER[BB],
              thunkedEqualizer: Ɛ.EQUALIZER[BBB]
            ): AA > BB =
              equalizingDot.crossPreRestrict[A, AA, B](
                source,
                thunkedEqualizer.asInstanceOf[Ɛ.EQUALIZER[B]].restrict(arrow)
              )

            override def apply(a: AA): BB = target.↔ / arrow(source.↔ \ a)

            override def o[ZZ <: ~~](that: ZZ > AA): ZZ > BB =
              that.preCompose[A, B, BB](this)

            override def preCompose[
              BBB <: Ɛ.~,
              C <: Ɛ.~,
              CC <: ~~
            ] (
              pre: ActionArrow[BBB, BB, C, CC]
            ): AA > CC = {
              val hackedPre = pre.asInstanceOf[ActionArrow[B, BB, C, CC]]
              ActionArrow(source, hackedPre.target, hackedPre.arrow o arrow)
            }

            override def toString = "ActionArrow[" + arrow + "]"

            override def equals(other: Any): Boolean = other match {
              case that: ActionArrow[A, AA, B, BB] =>
                that.source == source &&
                  that.target == target &&
                  that.arrow == arrow
            }

            override def hashCode = 0
          }

          private val memoizedDotWrapper =
            Memoize.generic withLowerBound[
              ({
                type λ[T <: Ɛ.~] = Action[T]
              }) # λ,
              ({
                type λ[T <: Ɛ.~] = DOT[Ɛ.VanillaWrapper[T]]
              }) # λ,
              Ɛ.~
            ] ActionDot.wrap

          override def makeDot[
            T <: Ɛ.~
          ] (
            predot: Action[T]
          ): DOT[Ɛ.VanillaWrapper[T]] =
            memoizedDotWrapper(predot)

          override def makeArrow[
            S <: Ɛ.~,
            T <: Ɛ.~
          ](
            prearrow: ActionPreArrow[S, T]
          ) = functionAsArrow(
            makeDot(prearrow.source),
            makeDot(prearrow.target),
            prearrow.function
          )

          override def functionAsArrow[
            S <: Ɛ.~,
            T <: Ɛ.~
          ] (
            source: DOT[Ɛ.VanillaWrapper[S]],
            target: DOT[Ɛ.VanillaWrapper[T]],
            f: S => T
          ): Ɛ.VanillaWrapper[S] > Ɛ.VanillaWrapper[T] = {
            val src = source.asInstanceOf[ActionDot[S, Ɛ.VanillaWrapper[S]]]
            val tgt = target.asInstanceOf[ActionDot[T, Ɛ.VanillaWrapper[T]]]
            ActionArrow[S, Ɛ.VanillaWrapper[S], T, Ɛ.VanillaWrapper[T]](
              src,
              tgt,
              src.action.carrier(
                tgt.action.carrier
              )(f)
            )
          }

          override def bifunctionAsBiArrow[
            L <: Ɛ.~,
            RIGHT <: Ɛ.~,
            T <: Ɛ.~
          ](
            left: DOT[Ɛ.VanillaWrapper[L]],
            right: DOT[Ɛ.VanillaWrapper[RIGHT]],
            target: DOT[Ɛ.VanillaWrapper[T]]
          )(
            bifunc: (L, RIGHT) => T
          ): BiArrow[Ɛ.VanillaWrapper[L], Ɛ.VanillaWrapper[RIGHT], Ɛ.VanillaWrapper[T]] = {
            val l = left.asInstanceOf[ActionDot[L, Ɛ.VanillaWrapper[L]]]
            val r = right.asInstanceOf[ActionDot[RIGHT, Ɛ.VanillaWrapper[RIGHT]]]
            val t = target.asInstanceOf[ActionDot[T, Ɛ.VanillaWrapper[T]]]
            (l x r).biArrow(t) { (a, b) =>
              Ɛ.VanillaWrapper(bifunc(a.element, b.element))
            }
          }
        }
  }

  trait ElementWrapper[
    A <: ~
  ] {
    val element: A
  }

  object VanillaWrapper {
    def ↔[
      A <: ~
    ] =
      new ↔[
        A,
        VanillaWrapper[A]
      ](
        a => VanillaWrapper(a),
        aa => aa.element
      )
  }

  case class VanillaWrapper[
    A <: ~
  ] (
    element: A
  ) extends ElementWrapper[
    A
  ]
}

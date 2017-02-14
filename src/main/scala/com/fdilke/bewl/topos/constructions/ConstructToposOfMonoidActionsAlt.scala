package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import com.fdilke.bewl.helper.{Memoize, ⊕}

trait ConstructToposOfMonoidActionsAlt extends
  BaseTopos with
  ToposEnrichments {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfMonoidActions {

    def of[
      M <: ~
    ] (
      monoid: Ɛ.Monoid[M]
    ) : Topos[~] =
      new Topos[~] {
        import monoid.{ carrier, action, Action }
        
          override type DOT[A <: ~] = ActionDotFacade[A]
          override type >[A <: ~, B <: ~] = ActionArrowFacade[A, B]
          override type UNIT = Ɛ.UNIT
          
          type IDEAL = M → Ɛ.TRUTH
          override type TRUTH = IDEAL

          override lazy val I = 
            ActionDot(Ɛ.I) {
              (i, m) => i
            }

          private object Ideals {
            private val possibleIdeals = carrier.power

            private val ideals =
              possibleIdeals.whereAll(carrier, carrier) {
                (f, m, n) => 
                  Ɛ.OmegaEnrichments(f(m)) → 
                    f(monoid.multiply(m, n))
              }

            def restrict[
              H <: ~
            ](
              that: Ɛ.DOT[H]
            )(
              bifunc: (H, M) => Ɛ.TRUTH
            ): Ɛ.>[H, IDEAL] =
              ideals.restrict(
                  possibleIdeals.transpose(that)(bifunc)
              )

            private val idealMultiply =
              restrict(ideals x carrier) {
                case (i ⊕ s, t) => 
                  ideals.inclusion(i)(
                      monoid.multiply(s, t)
                  )
              }

            val omega = 
              ActionDot[IDEAL](
                  ideals
              )(
                Ɛ.BiArrow[IDEAL, M, IDEAL](
                    ideals x carrier, 
                    idealMultiply
                )(_, _)
              )
          }
          
          override lazy val omega = 
            Ideals.omega
            
          override lazy val truth = 
            new ActionArrow[Ɛ.UNIT, IDEAL](
              I,
              omega,
              Ideals.restrict(Ɛ.I) {
                (i, m) => Ɛ truth i
              }
            )

          trait ActionDotFacade[
            S <: ~
          ] extends Dot[S] {
            val underlyingAction: Action[S] 
          }
            
          class ActionDot[
            S <: ~
          ](
            action: Action[S]
          ) extends ActionDotFacade[S] { actionDot =>
            override val underlyingAction =
              action // TODO: tidy up

            override def `>Uncached`[
              T <: ~
            ](
              that: ActionDotFacade[T]
            ): EXPONENTIAL[S,T] = 
              ???
              
            override def apply[
              T <: ~
            ](
                target: ActionDotFacade[T]
            )(
                f: S => T
            ): S > T = 
              new ActionArrow[S, T](
                actionDot,
                target,
                f
              )
              
            override lazy val globals: Traversable[UNIT > S] = {
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
                new ActionArrow(
                  I, 
                  actionDot, 
                  fixedPoints.inclusion o global
                )
              }
            }
              
            override def sanityTest(): Unit = {
              action.actionCarrier.sanityTest
              action.sanityTest
            }
              
            override lazy val toI: S > UNIT = 
              new ActionArrow(
                actionDot, 
                I, 
                action.actionCarrier.toI
              )
              
            override def xUncached[
              T <: ~
            ](
              that: ActionDotFacade[T]
            ): BIPRODUCT[S, T] = {
              val product: Ɛ.BIPRODUCT[S, T] =
                action.actionCarrier x that.underlyingAction.actionCarrier

              new ActionDot[S x T](
                monoid.action(product) {
                  case (s ⊕ t, m) => product.pair(
                    action.actionMultiply(s, m),
                    that.underlyingAction.actionMultiply(t, m)
                  )
                }
              ) with BiproductDot[S, T, S x T] {
                override val left = actionDot
                override val right = that
                def pair(l: S,r: T) = 
                  product.pair(l, r)
              }
            }
          }
          
          trait ActionArrowFacade[ // TODO: do we need this trait? Is there more than one implementation?
            S <: ~,
            T <: ~
          ] extends Arrow[S, T] {
            val arrow: Ɛ.>[S, T]
          }
          
          class ActionArrow[
            S <: ~,
            T <: ~
          ](
            val source: ActionDotFacade[S],
            val target: ActionDotFacade[T],
            override val arrow: Ɛ.>[S, T]
          ) extends ActionArrowFacade[S, T] {
            def this(
              source: ActionDotFacade[S],
              target: ActionDotFacade[T],
              function: S => T
            ) =
              this(
                  source,
                  target,
                  source.underlyingAction.carrier(
                      target.underlyingAction.carrier
                  )(
                      function
                  )
                )
                
            override def \[
              U <: ~
            ](
                monic: U > T
            ): S > U = 
              new ActionArrow[S, U](
                  source,
                  monic.source,
                  arrow \ monic.arrow
              )
              
            override def ?=(
              that: S > T
            ): EQUALIZER[S] = {
              val equalizer : Ɛ.EQUALIZER[S]= 
                arrow ?= that.arrow
              val equalizerCarrier =
                equalizer x carrier  
              val restrictedMultiply =
                equalizer.restrict(
                    equalizerCarrier.biArrow(source.underlyingAction.carrier) {
                      (s, m) => 
                        source.underlyingAction.actionMultiply(
                          equalizer.inclusion(s), 
                          m
                        )
                    }.arrow
                )
              new ActionDot(
                  action(
                      equalizer
                  ){ (s, m) =>
                      restrictedMultiply(
                          equalizerCarrier.pair(s, m)
                      )
                  }
              ) with EqualizingDot[S] { equalizingDot =>
                val equalizerTarget = source 
                def restrict[
                  R <: ~
                ](
                    actionArrow: ActionArrowFacade[R, S]
                ) = 
                  new ActionArrow[R, S](
                    actionArrow.source,
                    equalizingDot,
                    equalizer.restrict(
                      actionArrow.arrow
                    )
                  )
              }
            }
              
            override def apply(
              s: S
            ): T = 
              arrow(s)
              
            override lazy val chi: T > TRUTH = 
              new ActionArrow(
                target,
                omega,
                Ideals.restrict(target.underlyingAction.actionCarrier) {
                  (t, m) => arrow.chi(target.underlyingAction.actionMultiply(t, m))
                })
              
            override def o[
              R <: ~
            ](
              that: R > S
            ): R > T = 
              new ActionArrow[R, T](
                that.source,
                target,
                arrow o that.arrow
              )
              
            override def sanityTest() {
              arrow.sanityTest
              assert(
                monoid.actions.isMorphism(
                  source.underlyingAction, 
                  target.underlyingAction, 
                  arrow
                )
              )
            }
          }
          
          object ActionDot {
            def apply[
              A <: ~
            ](
              actionCarrier: Ɛ.DOT[A]
            )(
              actionMultiply: (A, M) => A
            ): ActionDot[
              A
            ] =
              new ActionDot(
                action(
                    actionCarrier
                )(
                    actionMultiply
                )
              )
          }
      }
    }
}

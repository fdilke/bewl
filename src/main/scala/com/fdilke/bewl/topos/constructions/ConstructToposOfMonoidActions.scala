package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicMachinery}
import com.fdilke.bewl.helper.{Memoize, ⊕}
import scala.language.higherKinds
import scala.language.reflectiveCalls

trait ConstructToposOfMonoidActions extends
  BaseTopos with
  ToposEnrichments with  
  ConstructDefaultMonoidAssistant {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfMonoidActions {
    def of[
      M <: ~
    ]( 
      monoid: Monoid[M]
    ) : Topos[~] with Wrappings[
      ~,
      ~,
      ({type λ[X <: ~] = monoid.Action[X]}) # λ,
      ({type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y]}) # λ,
      ({type λ[T <: ~] = T}) # λ
    ] =
      of(
        monoid, 
        monoidAssistant.asInstanceOf[
          MonoidAssistant[
            ({type λ[A <: ~]}) # λ
          ]
        ] /* TODO: shouldn't need this cast, but I'm b******d if I can fix it.
        		 Bug in type checker? */
      )
      
    def of[
      M <: ~,
      ACTION_ANALYSIS[Z <: ~]
    ] ( 
      monoid: Monoid[M],
      assistant: MonoidAssistant[ACTION_ANALYSIS] 
    ) : Topos[~] with Wrappings[
      ~,
      ~,
      ({type λ[X <: ~] = monoid.Action[X]}) # λ,
      ({type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y]}) # λ,
      ({type λ[T <: ~] = T}) # λ
    ] =
    new Topos[~] with Wrappings[
      ~,
      ~,
      ({type λ[X <: ~] = monoid.Action[X]}) # λ,
      ({type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y]}) # λ,
      ({type λ[T <: ~] = T}) # λ
    ] {
      import monoid.{ carrier, multiply, action, unit, Action }
      
     val analyzer: monoid.ActionAnalyzer[
        ({
          type λ[A <: ~] = 
            monoid.MonoidSpecificActionAnalysis[A] with ACTION_ANALYSIS[A]  
        }) # λ
     ] =
        assistant.actionAnalyzer(monoid)
      override type DOT[A <: ~] = ActionDot[A]
      override type >[A <: ~, B <: ~] = ActionArrow[A, B]
      override type UNIT = Ɛ.UNIT
      override type →[T <: ~, U <: ~] = Ɛ.→[Ɛ.x[M, T], U]
      
      type IDEAL = Ɛ.→[M, Ɛ.TRUTH]
      override type TRUTH = IDEAL
  
      override lazy val I = 
        new ActionDot(
          action(Ɛ.I) {
            (i, m) => i
          }
        )
  
      private object Ideals {
        private val possibleIdeals = carrier.power
        import possibleIdeals.{ evaluate => $ }
  
        private val ideals =
          possibleIdeals.whereAll(carrier, carrier) {
            (f, m, n) => 
              Ɛ.OmegaEnrichments($(f, m)) → 
                $(f, monoid.multiply(m, n))
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
              $(ideals.inclusion(i),
                  monoid.multiply(s, t)
              )
          }
  
          val omega = 
            new ActionDot[IDEAL](
              Action(
                ideals,
              Ɛ.BiArrow[IDEAL, M, IDEAL](
                  ideals x carrier, 
                  idealMultiply
              )
            )
          )
        }
            
        override lazy val omega = 
          Ideals.omega
          
        override lazy val truth = 
          ActionArrow[Ɛ.UNIT, IDEAL](
            I,
            omega,
            Ideals.restrict(Ɛ.I) {
              (i, m) => Ɛ truth i
            }
          )
  
        class ActionDot[
          S <: ~
        ](
          val action: Action[S]
        ) extends Dot[S] { actionDot =>
          override def `>Uncached`[
            T <: ~
          ](
            that: ActionDot[T]
          ): EXPONENTIAL[S,T] = 
            {
              val mXs = carrier x action.carrier
              val possibleMorphisms = 
                mXs > that.action.actionCarrier
                
              import possibleMorphisms.{ evaluate => $ }

              val morphisms: Ɛ.EQUALIZER[S → T] =
                possibleMorphisms.whereAll(
                  carrier,
                  carrier,
                  action.actionCarrier
                ) {
                  (f, n, m, s) =>
                    that.action.actionCarrier.=?=(
                      $(f, 
                        mXs.pair(
                          multiply(m, n),
                          action.actionMultiply(s, n)
                        )
                      ),
                      that.action.actionMultiply(
                        $(f, 
                          mXs.pair(m, s)
                        ),
                        n
                      )
                    )
                }

              val morphismMultiply  =
                morphisms.restrict(
                  possibleMorphisms.transpose(
                    morphisms x carrier
                  ) {
                    case (f ⊕ m, n ⊕ s) =>
                      $(morphisms.inclusion(f),
                        mXs.pair(
                          multiply(m, n), 
                          s
                        )
                      )
                  }
                )
              
              new ActionDot[S → T] (
                Action(
                  morphisms, 
                  Ɛ.BiArrow[S → T, M, S → T](
                    morphisms x carrier,
                    morphismMultiply
                  )
                )
              ) with ExponentialDot[
                S, 
                T, 
                S → T
              ] { exponentialDot =>
                override val source = actionDot 
                override val target = that 
                override def evaluate(
                  function: S → T,
                  s: S
                ): T = 
                  $(
                    function,
                    mXs.pair(
                      unit(
                        actionDot.action.carrier.toI(s)
                      ),
                      s
                    )
                  )
                  
                override def transpose[
                  R <: ~
                ](
                  biArrow: BiArrow[R, S, T]
                ): ActionArrow[R, S → T] = {
                  val rAction = biArrow.product.left

                  ActionArrow(
                    rAction,
                    exponentialDot,
                    morphisms.restrict(
                      possibleMorphisms.transpose(
                        rAction.action.actionCarrier
                      ) {
                        case (r, m ⊕ s) => 
                          biArrow(
                            rAction.action.actionMultiply(r, m),
                            s
                          )
                      }
                    )
                  )
                }
              }
          }
            
          override def apply[
            T <: ~
          ](
            target: ActionDot[T]
          )(
              f: S => T
          ): S > T = 
            ActionArrow[S, T](
              actionDot,
              target,
              action.carrier(
                target.action.carrier
              ) { f }
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
              ActionArrow(
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
            ActionArrow(
              actionDot, 
              I, 
              action.actionCarrier.toI
            )
            
          override def xUncached[
            T <: ~
          ](
            that: ActionDot[T]
          ): BIPRODUCT[S, T] = {
            val product: Ɛ.BIPRODUCT[S, T] =
              action.actionCarrier x that.action.actionCarrier
  
            new ActionDot[S x T](
              monoid.action(product) {
                case (s ⊕ t, m) => product.pair(
                  action.actionMultiply(s, m),
                  that.action.actionMultiply(t, m)
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
        
        case class ActionArrow[
          S <: ~,
          T <: ~
        ](
          source: ActionDot[S],
          target: ActionDot[T],
          arrow: Ɛ.>[S, T]
        ) extends Arrow[S, T] {
          def this(
            source: ActionDot[S],
            target: ActionDot[T],
            function: S => T
          ) =
            this(
              source,
              target,
              source.action.carrier(
                target.action.carrier
              )(
                function
              )
            )
              
          override def \[
            U <: ~
          ](
            monic: U > T
          ): S > U = 
            ActionArrow[S, U](
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
                  equalizerCarrier.biArrow(
                    source.action.carrier
                  ) {
                    (s, m) => 
                      source.action.actionMultiply(
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
                actionArrow: ActionArrow[R, S]
              ) = 
                ActionArrow[R, S](
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
            ActionArrow(
              target,
              omega,
              Ideals.restrict(
                target.action.actionCarrier
              ) {
                (t, m) => arrow.chi(
                  target.action.actionMultiply(t, m)
                )
              }
            )
            
          override def o[
            R <: ~
          ](
            that: R > S
          ): R > T = 
            ActionArrow[R, T](
              that.source,
              target,
              arrow o that.arrow
            )
            
          override def sanityTest() {
            arrow.sanityTest
            assert(
              monoid.actions.isMorphism(
                source.action, 
                target.action, 
                arrow
              )
            )
          }
        }
        
        override def bifunctionAsBiArrow[
          L <: ~, 
          R <: ~, 
          T <: ~
        ](
          left: ActionDot[L], 
          right: ActionDot[R], 
          target: ActionDot[T]
        )(
          bifunc: (L, R) ⇒ T
        ): BiArrow[L,R,T] = // no wrappings: generic version will do
          (left x right).biArrow(target) { bifunc }
          
        override def functionAsArrow[
          S <: ~, 
          T <: ~
        ]( 
          source: ActionDot[S], 
          target: ActionDot[T], 
          f: S ⇒ T
        ): ActionArrow[S,T] = // no wrappings: generic version will do
          source(target){ f }
        
        override def makeArrow[
          S <: ~, 
          T <: ~
        ](
          preArrow: monoid.ActionPreArrow[S,T]
        ): ActionArrow[S,T] =
          ActionArrow[S, T](
            makeDot(preArrow.source),
            makeDot(preArrow.target),
            preArrow.source.actionCarrier(
              preArrow.target.actionCarrier
            ) {
              preArrow.function
            }
          )

        private val memoizedDotWrapper =
          Memoize.generic withLowerBound[
            ({
              type λ[T <: Ɛ.~] = Action[T]
            }) # λ,
            ({
              type λ[T <: Ɛ.~] = DOT[T]
            }) # λ,
            Ɛ.~
          ] { action => 
            new ActionDot(action) 
          }
            
        override def makeDot[T <: ~](
          predot: monoid.Action[T]
        ): ActionDot[T] = 
          memoizedDotWrapper(predot)
          
        override def unwrap[T <: ~](
          dot: ActionDot[T]
        ) =
          dot.action
      }
  }
}

package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.↔
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
            ???

          trait ActionDotFacade[
            S <: ~
          ] extends Dot[S] 
            
          class ActionDot[
            S <: ~
          ](
              action: Action[S]
          ) extends ActionDotFacade[S] {
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
              ???
              
            override lazy val globals: Traversable[UNIT > S] = 
              ???
              
            override def sanityTest(): Unit = 
              ???
              
            override lazy val toI: S > UNIT = 
              ???
              
            override def xUncached[
              T <: ~
            ](
                that: ActionDotFacade[T]
            ): BIPRODUCT[S, T] = 
              ???
          }
          
          trait ActionArrowFacade[
            S <: ~,
            T <: ~
          ] extends Arrow[S, T] 
          
          class ActionArrow[
            S <: ~,
            T <: ~
          ](
              val source: ActionDotFacade[S],
              val target: ActionDotFacade[T],
              val function: S => T
          ) extends ActionArrowFacade[S, T] {
            override def \[
              U <: ~
            ](
                monic: U > T
            ): S > U = 
              ???
              
            override def ?=(
              that: S > T
            ): EQUALIZER[S] = 
              ???
              
            override def apply(
              s: S
            ): T = 
              ???
              
            override lazy val chi: T > TRUTH = 
              ???
              
            override def o[
              R <: ~
            ](
              that: R > S
            ): R > T = 
              ???
              
            override def sanityTest() = 
              ???
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

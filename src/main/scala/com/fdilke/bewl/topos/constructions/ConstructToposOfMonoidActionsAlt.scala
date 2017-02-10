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
          override type DOT[A <: ~] = ActionDotFacade[A]
          override type >[A <: ~, B <: ~] = ActionArrowFacade[A, B]
          override type UNIT = Ɛ.UNIT

          override val I = 
            ??? 
            
          override val omega = 
            ???
            
          override val truth = 
            ???

          trait ActionDotFacade[
            S <: ~
          ] extends Dot[S] 
            
          class ActionDot[
            S <: ~
          ](
              action: monoid.Action[S]
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
              
            override val globals: Traversable[UNIT > S] = 
              ???
              
            override def sanityTest(): Unit = 
              ???
              
            override val toI: S > UNIT = 
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
              
            override val chi: T > TRUTH = 
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
      }
    }
}

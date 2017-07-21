package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos._
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import scala.language.higherKinds
import scala.language.reflectiveCalls

trait ConstructDefaultMonoidAssistant extends
  BaseTopos with
  ToposEnrichments {

  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  class ActionAnalysis[
    A <: ~
  ] {
    
  }
    
  trait MonoidAssistant[
    ACTION_ANALYSIS[A <: ~] <: ActionAnalysis[A]
  ] {
    def actionAnalyzer[
      M <: ~
    ] (
      monoid: Monoid[M]
    ): monoid.ActionAnalyzer[
        ({
          type λ[A <: ~] = 
            monoid.MonoidSpecificActionAnalysis[A] with ACTION_ANALYSIS[A]  
        }) # λ
    ]
  }

  object DefaultMonoidAssistant extends MonoidAssistant[
    ActionAnalysis
  ] {
    override def actionAnalyzer[
      M <: ~
    ] (
      monoid: Monoid[M]
    ) =
      new monoid.ActionAnalyzer[
        ({
          type λ[A <: ~] = 
            ActionAnalysis[A] with monoid.MonoidSpecificActionAnalysis[A]    
        }) # λ
      ] {
        override def analyze[A <: ~](
          action: monoid.Action[A]
        ) = 
          new ActionAnalysis[A] with monoid.MonoidSpecificActionAnalysis[A] {
            override def morphismsTo[B <: ~](
              target: monoid.Action[B] 
            ): Traversable[A > B] =
              ???
          }
    }
  }
  
  val monoidAssistant: MonoidAssistant[ActionAnalysis] = 
    DefaultMonoidAssistant    
}
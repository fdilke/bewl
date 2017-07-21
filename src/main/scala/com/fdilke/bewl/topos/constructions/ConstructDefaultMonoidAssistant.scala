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
    ): monoid.ActionAnalyzer[ACTION_ANALYSIS]
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
        ActionAnalysis
      ] {
        override def analyze[A <: ~](
          action: monoid.Action[A]
        ) = 
          new ActionAnalysis[A] {
          
          }
    }
  }
  
  val monoidAssistant: MonoidAssistant[ActionAnalysis] = 
    DefaultMonoidAssistant    
}
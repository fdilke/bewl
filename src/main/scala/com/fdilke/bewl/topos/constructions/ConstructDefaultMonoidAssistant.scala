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

    // TODO fix 'bakery of doom' issue
    // and put the monoid assistant code in here

  trait MonoidAssistant {
    def actionAnalyzer[
      M <: ~
    ] (
      monoid: Monoid[M]
    ): monoid.ActionAnalyzer[ActionAnalysis]
  }
    
  object DefaultMonoidAssistant extends MonoidAssistant {
    override def actionAnalyzer[
      M <: ~
    ] (
      monoid: Ɛ.Monoid[M]
    ): monoid.ActionAnalyzer[ActionAnalysis] =
      null // :( :( :(
  }
  
  val monoidAssistant : MonoidAssistant = 
    DefaultMonoidAssistant    
}
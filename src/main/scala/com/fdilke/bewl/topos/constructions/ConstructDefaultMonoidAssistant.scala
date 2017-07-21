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

  trait GenericAny[
    A <: ~
  ] // TODO: abominable concept, shouldn't need this (type system limitations?)
    
  trait MonoidAssistant[
    ACTION_ANALYSIS[A <: ~] 
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
    GenericAny
  ] {
    override def actionAnalyzer[
      M <: ~
    ] (
      monoid: Monoid[M]
    ) =
      new monoid.ActionAnalyzer[
        ({
          type λ[A <: ~] = 
            monoid.MonoidSpecificActionAnalysis[A] with GenericAny[A]    
        }) # λ
      ] {
        override def analyze[A <: ~](
          action: monoid.Action[A]
        ) = 
          new monoid.MonoidSpecificActionAnalysis[A] with GenericAny[A] {
            override def morphismsTo[B <: ~](
              target: monoid.Action[B] 
            ): Traversable[A > B] =
              ???
          }
    }
  }
  
  val monoidAssistant: MonoidAssistant[GenericAny] = 
    DefaultMonoidAssistant    
}
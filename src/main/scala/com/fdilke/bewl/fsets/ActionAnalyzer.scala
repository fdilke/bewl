package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >, Monoid, makeDot}
import scala.language.postfixOps

object ActionAnalyzer {
  trait Ingredients[M, S ] {
    val monoid: Monoid[M]
    val action: monoid.Action[S]
  } 

  // Synthetic curried constructor with dependent types
  def apply[M, S](
    _monoid: Monoid[M]
  ) (
    _action: _monoid.Action[S]
  ) = new ActionAnalyzer[M, S] (
      new Ingredients[M, S] {
        override val monoid = _monoid
        override val action = _action.asInstanceOf[monoid.Action[S]]
      }
    )
}

class ActionAnalyzer[M, S]( 
  ingredients: ActionAnalyzer.Ingredients[M, S] 
){
  import ingredients._
  
  private val monoidElements = 
    elementsOf(monoid.carrier)

  private val actionElements = 
    elementsOf(action.actionCarrier)

  case class Cyclic(
     generator: S
  ) {
   val elements: Set[S] =
      monoidElements map { 
        action.actionMultiply(
          generator, 
          _
        )
      } toSet
    
    def contains(s: S) =
      elements contains s

    def subsetOf(
      other: Cyclic
    ) =
      elements.subsetOf(
        other.elements          
      )
  }
  
  class MaximalCyclics(
    val cyclics: Seq[Cyclic] =
      Seq.empty
  ) { self =>
    def contains(s: S) =
      cyclics.exists { 
        _.contains(s) 
      }
    
    def +(
      newCyclic: Cyclic
    ) = 
      new MaximalCyclics(
        newCyclic +: ( 
          cyclics filterNot { 
            _.subsetOf(newCyclic)
          } 
        )
      )

    def transversal =
      cyclics map {
        _.generator
      }
    
    def <<(s: S): MaximalCyclics = 
      if (self.contains(s))
        self
      else 
        self + Cyclic(s)
  }
    
  lazy val extractGenerators: S > S = 
    makeDot(
      actionElements.foldLeft(
        new MaximalCyclics
      ) { 
        _ << _
      }.transversal
    ) (
      action.actionCarrier
    ) {
      identity[S]
    }
}
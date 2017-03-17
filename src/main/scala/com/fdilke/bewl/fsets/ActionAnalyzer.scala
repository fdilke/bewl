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
    
  class MaximalCyclics(
    val cyclics: Seq[Set[S]] =
      Seq.empty
  ) { self =>
    def contains(s: S) =
      cyclics.exists { 
        _.contains(s) 
      }
    
    def +(
      newCyclic: Set[S]
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
        _.head
      }
    
    def <<(s: S): MaximalCyclics = 
      if (self.contains(s))
        self
      else 
        self + 
          monoidElements.map { 
            action.actionMultiply(s, _)
          }.toSet
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
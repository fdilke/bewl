package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >, Monoid, makeDot}
import scala.language.postfixOps
import scala.language.higherKinds

object ActionAnalyzer {
  trait Ingredients[M, A] {
    val monoid: Monoid[M]
    val action: monoid.Action[A]
  } 

  // Synthetic curried constructor with dependent types
  def apply[M, A](
    _monoid: Monoid[M]
  ) (
    _action: _monoid.Action[A]
  ) = new ActionAnalyzer[
    M, 
    A 
  ] (
      new Ingredients[M, A] {
        override val monoid = _monoid
        override val action = 
          _action.asInstanceOf[monoid.Action[A]]
      }
    )
}

class ActionAnalyzer[
  M, 
  A 
]( 
  ingredients: ActionAnalyzer.Ingredients[M, A] 
){
  import ingredients._
  
  private val monoidElements = 
    elementsOf(monoid.carrier)

  private val actionElements = 
    elementsOf(action.actionCarrier)

  case class Cyclic(
     generator: A
  ) {
   val elements: Set[A] =
      monoidElements map { 
        action.actionMultiply(
          generator, 
          _
        )
      } toSet
    
    def contains(a: A) =
      elements contains a

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
    def contains(a: A) =
      cyclics.exists { 
        _ contains a 
      }
    
    def +(
      newCyclic: Cyclic
    ) = 
      new MaximalCyclics(
        newCyclic +: ( 
          cyclics filterNot { 
            _ subsetOf newCyclic
          } 
        )
      )

    def transversal =
      cyclics map {
        _.generator
      }
    
    def <<(a: A): MaximalCyclics = 
      if (self contains a)
        self
      else 
        self + Cyclic(a)
  }
    
  lazy val extractGenerators: A > A = 
    makeDot(
      actionElements.foldLeft(
        new MaximalCyclics
      ) { 
        _ << _
      }.transversal
    ) (
      action.actionCarrier
    ) {
      identity[A]
    }
    
  def morphismsTo[B](
    target: monoid.Action[B]
  ) =
    new Traversable[A > B] {
      override def foreach[U](
        f: (A > B) => U
      ) {
        ???
      }
    }
}
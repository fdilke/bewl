package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >, Monoid, makeDot}
import scala.language.postfixOps
import scala.language.higherKinds
import scala.language.existentials

object FiniteSetsMonoidAnalyzer {
  def apply[M](
    monoid: Monoid[M]
  ) = 
    new monoid.Analyzer { 
    private val monoidElements = 
      elementsOf(monoid.carrier)
  
    def actionAnalyzer[
      A 
    ]( 
      action: monoid.Action[A]
    ) = {
      val actionElements = 
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
      ) extends action.BaseMaximalCyclics[
        MaximalCyclics
      ] { self =>
        override def contains(a: A) =
          cyclics.exists { 
            _ contains a 
          }
        
        private def +(
          newCyclic: Cyclic
        ) = 
          new MaximalCyclics(
            newCyclic +: ( 
              cyclics filterNot { 
                _ subsetOf newCyclic
              } 
            )
          )
    
        override def +(
          a: A
        ) = 
          this + Cyclic(a)
          
        def transversal =
          cyclics map {
            _.generator
          }
        
        override def <<(a: A): MaximalCyclics = 
          if (self contains a)
            self
          else 
            self + Cyclic(a)
      }

      new action.Analyzer[
        MaximalCyclics,
        monoid.Action
      ] {
        override val initialCyclics: MaximalCyclics =
          new MaximalCyclics
          
        override lazy val generators =
          actionElements.foldLeft(
            initialCyclics
          ) { 
            _ << _
          }.transversal
          
        override def morphismsTo[B](
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
    }
  }
}
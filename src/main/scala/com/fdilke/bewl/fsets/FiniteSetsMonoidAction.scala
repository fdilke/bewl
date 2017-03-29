package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >}
import scala.language.higherKinds
import scala.language.postfixOps

// In a couple of places here, I gave up on keeping the type system happy and used casts :( 
// - the 'boilerplate curried constructor' seems to be a necessary evil
// - when passing a parallel action, the type system doesn't know it's for the same monoid
// A proper fix for these is eagerly awaited.

object FiniteSetsMonoidAction {
  def apply[M, A](
    _monoid: FiniteSets.Monoid[M]
  )(
    _action: _monoid.Action[A]
  ) = 
    new FiniteSetsMonoidAction[
      M, 
      A
    ] {
      override val monoid = _monoid
      override val action = _action.asInstanceOf[monoid.Action[A]]
    }
}

trait FiniteSetsMonoidAction[
  M,
  A
] {
  val monoid: FiniteSets.Monoid[M]
  val action: monoid.Action[A]
  
  private lazy val monoidElements = 
    elementsOf(monoid.carrier)

  private lazy val actionElements = 
    elementsOf(action.carrier)
    
  case class Cyclic(
     generator: A
  ) {
   val uu: FiniteSets.Monoid[M]#Action[_] = action 
   val tt: FiniteSets.BiArrow[A, M, A] = action.actionMultiply
   
   val elements: Set[A] =
      monoidElements map { (m : M) =>
        action.actionMultiply(
          generator, 
          m
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

    def +(
      a: A
    ): MaximalCyclics = 
      self + Cyclic(a)
      
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
  
  lazy val initialCyclics = 
    new MaximalCyclics
    
  lazy val generators =
    actionElements.foldLeft(
      initialCyclics
    ) { 
      _ << _
    }.transversal

  def morphismsTo[B](
    _target: FiniteSets.Monoid[M]#Action[B] 
  ) = {
    val target: monoid.Action[B] = 
      _target.asInstanceOf[monoid.Action[B]]
    
    new Traversable[A > B] {
      override def foreach[U](
        f: (A > B) => U
      ) {
        ???
      }
    }  
  }
}
package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.>
import FiniteSetsActionAssistant.extractGenerators

import org.scalatest.FreeSpec
import org.scalatest.Matchers._


class ActionAnalyzerTest extends FreeSpec {
  
  private val (i, x, y) = ('i, 'x, 'y)

  private val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

  import monoidOf3.regularAction
    
  private type M = Symbol

  private val analyzer = 
    ActionAnalyzer(
      monoidOf3
    )(
      regularAction
    )
    
  "The action analyzer" - {
    "can build up a set of maximal cyclic subalgebras for a monoid action" - {

      "which are initially empty" in {
        val cyclics =
          new analyzer.MaximalCyclics
          
        cyclics.cyclics shouldBe empty
        
        cyclics.contains(i) shouldBe false
        
        cyclics.transversal shouldBe empty
      }
      
      "which can be added to, filtering out any eclipsed cyclics" in {
        val cyclics_I =
          new analyzer.MaximalCyclics + Set(i)

        cyclics_I.cyclics should have size 1
        
        val cyclics_I_X_Y =
          new analyzer.MaximalCyclics + Set(i) + Set(x) + Set(y)

        cyclics_I_X_Y.cyclics should have size 3
        
        (cyclics_I_X_Y + Set(i, y)).cyclics shouldBe Seq(
          Set(i, y),
          Set(x)
        )
      }
      
      "which can be used to build up the complete set" in {
        val allMaxCyclics =
          Seq(i, x, y).foldLeft(
            new analyzer.MaximalCyclics
          ) { 
            _ << _
          }
        
        allMaxCyclics.cyclics shouldBe Seq(
          Set(i, x, y)
        )
      }
      
//      cyclics.cy        
//      generators shouldBe 'monic
//      val generatingElements = 
//        elementsOf(
//          generators.source
//        )        
      
//      generatingElements should have size 1
//      
//      // Check it's one of the elements that can BE a generator
//      Set[M]() should contain (
//        generatingElements.head
//      )
    }
  }
}
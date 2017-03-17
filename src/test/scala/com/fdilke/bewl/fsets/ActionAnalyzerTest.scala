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

  import analyzer.{ MaximalCyclics, Cyclic }
  
  "The action analyzer" - {
    "can build up a set of maximal cyclic subalgebras for a monoid action" - {

      "which are initially empty" in {
        val cyclics =
          new MaximalCyclics
          
        cyclics.cyclics shouldBe empty
        
        cyclics.contains(i) shouldBe false
        
        cyclics.transversal shouldBe empty
      }
      
      "which can be added to, filtering out any eclipsed cyclics" in {
        val cyclics_I =
          new MaximalCyclics + Cyclic(i)

        cyclics_I.cyclics should have size 1
        
        val cyclics_X_Y =
          new MaximalCyclics + Cyclic(x) + Cyclic(y)

        cyclics_X_Y.cyclics should have size 1
        
        (cyclics_X_Y + Cyclic(i)).cyclics shouldBe Seq(
          Cyclic(i)
        )
      }
      
      "which can be used to build up the complete set" in {
        val allMaxCyclics =
          Seq(i, x, y).foldLeft(
            new MaximalCyclics
          ) { 
            _ << _
          }
        
        allMaxCyclics.cyclics shouldBe Seq(
          Cyclic(i)
        )
      }
    }
  }
}
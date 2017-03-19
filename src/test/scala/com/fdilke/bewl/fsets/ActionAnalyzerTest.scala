package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{UNIT, >, ToposOfMonoidActions }
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

  def analyzerFor[A](
    action: monoidOf3.Action[A]
  ) =
    ActionAnalyzer(
      monoidOf3
    )(
      action
    )

  private val analyzer = 
    analyzerFor(
      regularAction
    )

  import analyzer.{ MaximalCyclics, Cyclic }
  
  private val actionTopos = 
      ToposOfMonoidActions of monoidOf3
  
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

      "as expected for the empty action" in {
        val emptyAction =
          actionTopos.unwrap(
            actionTopos.O
          )
        val emptyAnalyzer = 
          analyzerFor(
            emptyAction
          )
        elementsOf(
          emptyAnalyzer.extractGenerators.source
        ) shouldBe empty
      }
      
      "as expected for a non-cyclic action" in {
        val regularSquared =
          actionTopos.unwrap(
            actionTopos.makeDot(
              regularAction
            ).squared
          )
        val squareAnalyzer = 
          analyzerFor(
            regularSquared
          )
        elementsOf(
          squareAnalyzer.extractGenerators.source
        ) should have size 7
      }
      
      "as expected for another non-cyclic action" in {
        val theOmega = 
          actionTopos.unwrap(
            actionTopos.omega
          )
        val omegaAnalyzer =
          analyzerFor(
            theOmega
          )
        elementsOf(
          omegaAnalyzer.extractGenerators.source
        ) should have size 2
      }
    }
    
    "can enumerate the morphisms into another action" - {
      "for the trivial action" in {
        val otherAction: monoidOf3.Action[actionTopos.UNIT] =
          actionTopos.unwrap(
            actionTopos.I
          )
//        val morphisms =
//          analyzer.morphismsTo(
//            otherAction
//          ) 
//          
//        morphisms should have size 1
//        val morphism: M > UNIT =
//          morphisms.head
//        morphism should have {
//          'source(regularAction.actionCarrier)
//          'target(I)
//        }
//        morphism.sanityTest
      }
    }
  }
}
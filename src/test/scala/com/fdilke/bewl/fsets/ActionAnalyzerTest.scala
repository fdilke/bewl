package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{UNIT, >, ToposOfMonoidActions }
import FiniteSetsActionAssistant.generators

import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.language.reflectiveCalls
import scala.language.existentials

class ActionAnalyzerTest extends FreeSpec {
  
  private val (i, x, y) = ('i, 'x, 'y)

  private val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

  import monoidOf3.regularAction

  private val monoidAnalyzer =
    FiniteSetsMonoidAnalyzer(
      monoidOf3
    )
        
  private type M = Symbol

  def analyzerFor[A](
    action: monoidOf3.Action[A]
  ) =
    monoidAnalyzer.actionAnalyzer(
      action
    )

  private val analyzer = 
    analyzerFor(
      regularAction
    )

  import analyzer.{ initialCyclics }
  
  private val actionTopos = 
      ToposOfMonoidActions of monoidOf3
  
  "The action analyzer" - {
    "can build up a set of maximal cyclic subalgebras for a monoid action" - {

      "which are initially empty" in {
        val cyclics =
          analyzer.initialCyclics
          
        cyclics.cyclics shouldBe empty
        
        cyclics.contains(i) shouldBe false
        
        cyclics.transversal shouldBe empty
      }
      
      "which can be added to, filtering out any eclipsed cyclics" in {
        val cyclics_I =
          initialCyclics + i

        cyclics_I.cyclics should have size 1
        
        val cyclics_X_Y =
          initialCyclics + x + y

        cyclics_X_Y.cyclics should have size 1
        
        val theCyclics = 
          (cyclics_X_Y + i).cyclics 
          
        theCyclics should have size 1
        theCyclics.head.generator shouldEqual i
      }
      
      "which can be used to build up the complete set" in {
        val allMaxCyclics =
          Seq(i, x, y).foldLeft(
            initialCyclics
          ) { 
            _ << _
          }
        
        val theCyclics =
          allMaxCyclics.cyclics 
          
        theCyclics should have size 1
        theCyclics.head.generator shouldEqual i
      }

      "as expected for the empty action" in {
        val emptyAction =
          actionTopos.unwrap(
            actionTopos.O
          )
        analyzerFor(
          emptyAction
        ).generators shouldBe empty
      }
      
      "as expected for a non-cyclic action" in {
        val regularSquared =
          actionTopos.unwrap(
            actionTopos.makeDot(
              regularAction
            ).squared
          )
        analyzerFor(
          regularSquared
        ).generators should have size 7
      }
      
      "as expected for another non-cyclic action" in {
        val theOmega = 
          actionTopos.unwrap(
            actionTopos.omega
          )
        analyzerFor(
          theOmega
        ).generators should have size 2
      }
    }
    
    "can enumerate the morphisms into another action" - {
      "for the trivial action" ignore {
        val otherAction: monoidOf3.Action[actionTopos.UNIT] =
          actionTopos.unwrap(
            actionTopos.I
          )
        val morphisms =
          analyzer.morphismsTo(
            otherAction
          ) 
          
        morphisms should have size 1
        val morphism: M > actionTopos.UNIT =
          morphisms.head
        morphism should have {
          'source(regularAction.actionCarrier)
          'target(
            actionTopos.unwrap(
              actionTopos.I
            )
          )
        }
        morphism.sanityTest
      }
    }
  }
}
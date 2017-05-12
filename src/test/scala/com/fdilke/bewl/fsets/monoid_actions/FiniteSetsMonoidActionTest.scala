package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.language.reflectiveCalls
import scala.language.existentials

import FiniteSets.{>, ToposOfMonoidActions}

class FiniteSetsMonoidActionTest extends FreeSpec {
  
  private val (i, x, y) = ('i, 'x, 'y)

  private val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

  import monoidOf3.regularAction
  
  private type M = Symbol

  private def analyzerFor[A](
    action: monoidOf3.Action[A]
  ): AbstractActionAnalysis[Symbol, A] with monoidOf3.MorphismEnumerator[A] =
    FiniteSetsMonoidAction(
      monoidOf3
    ).analyze(
      action
    )

  private val analyzer = 
    analyzerFor(
      regularAction
    )

  private val actionTopos = 
    ToposOfMonoidActions of monoidOf3

  import analyzer.initialCyclics
      
  "The action analyzer" - {
    "can build up a set of maximal cyclic subalgebras for a monoid action" - {

      "which are initially empty" in {
          import analyzer.initialCyclics
          
        initialCyclics.cyclics shouldBe empty
        
        initialCyclics.contains(i) shouldBe false
        
        initialCyclics.transversal shouldBe empty
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
    
    "can extract a set of generators for a monoid" in {
      import monoidOf3.regularAction

      FiniteSetsMonoidAction(
        monoidOf3
      ).analyze(
        regularAction
      ).generators shouldBe Seq(i)
    }

    "can extract a presentation" - {
      "for the regular monoid action" in {
        canExtractPresentation(monoidOf3.regularAction)
      }
      "for an empty monoid action" in {
        canExtractPresentation(        
            actionTopos.unwrap(
              actionTopos.O
            )
          )
      }
      "for the truth object monoid action" in {
        canExtractPresentation(        
            actionTopos.unwrap(
              actionTopos.omega
            )
          )
      }
      "for a more fancy monoid action" in {
        canExtractPresentation(        
            actionTopos.unwrap(
              actionTopos.omega x
                actionTopos.makeDot(
                  monoidOf3.regularAction
                )
            )
          )
      }
    // TODO: add more tests like this!
    }
     
    "can enumerate the morphisms into another action" - {
      "for the trivial action" in {
        val trivialAction: monoidOf3.Action[actionTopos.UNIT] =
          actionTopos.unwrap(
            actionTopos.I
          )
        val morphisms =
          analyzer.morphismsTo(
            trivialAction
          ) 
          
        morphisms should have size 1
        val morphism: M > actionTopos.UNIT =
          morphisms.head
        morphism should have {
          'source(regularAction.actionCarrier)
          'target(
            trivialAction.actionCarrier
          )
        }
        monoidOf3.actions.isMorphism(
          regularAction, 
          trivialAction, 
          morphism
        ) shouldBe true
        morphism.sanityTest
      }
    }
  }

  private def canExtractPresentation[A](
      action: monoidOf3.Action[A]
    ) {
      val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
        FiniteSetsMonoidAction(
          monoidOf3
        ).analyze(
          action
        ).generatorsWithRelators

      val presentedAction =
        FiniteSetsPresentedAction(
          monoidOf3
        )(
          generatorsWithRelators
        )
      presentedAction.sanityTest

      // Check this presents the original action
	    val theProjection: Int > A = 
	      presentedAction.project(
          action,
          generatorsWithRelators map { _.generator }
        )
        
      monoidOf3.actions.isMorphism(
         presentedAction.action,
         action,
         theProjection
      ) shouldBe true
    }
}
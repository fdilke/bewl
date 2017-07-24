package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{ functionAsArrow, LocalMonoidAssistant }
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.language.reflectiveCalls
import scala.language.existentials

import FiniteSets.{>, ToposOfMonoidActions}
import scala.language.postfixOps
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.fsets.AbstractActionAnalysis

class FiniteSetsMonoidActionTest extends FreeSpec {
  
  private val (i, x, y) = ('i, 'x, 'y)

  private val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators

  import monoidOf3.regularAction
  
  private def analysisFor[A](
    action: monoidOf3.Action[A]
  ): AbstractActionAnalysis[Symbol, A] with 
    monoidOf3.MorphismEnumerator[A] =
    LocalMonoidAssistant.actionAnalyzer(
      monoidOf3
    ).analyze(
      action
    )

  private val regularAnalysis = 
    analysisFor(
      regularAction
    )

  private val actionTopos = 
    ToposOfMonoidActions of monoidOf3

  import regularAnalysis.initialCyclics
      
  "The action analyzer" - {
    "can build up a set of maximal cyclic subalgebras for a monoid action" - {

      "which are initially empty" in {
        import regularAnalysis.initialCyclics
          
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
        analysisFor(
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
        analysisFor(
          regularSquared
        ).generators should have size 7
      }
      
      "as expected for another non-cyclic action" in {
        val theOmega = 
          actionTopos.unwrap(
            actionTopos.omega
          )
        analysisFor(
          theOmega
        ).generators should have size 2
      }
    }
    
    "can extract a set of generators for a monoid" in {
      analysisFor(
        monoidOf3.regularAction
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
          regularAnalysis.morphismsTo(
            trivialAction
          ) 
          
        morphisms should have size 1
        val morphism: Symbol > actionTopos.UNIT =
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
    "for the regular action to itself" in {
      val morphisms =
        regularAnalysis.morphismsTo(
          regularAction
        ) 
        
      morphisms should have size 3
      
      def leftMultiplication(
        l: Symbol
      ) =
        functionAsArrow[Symbol, Symbol](
          regularAction.actionCarrier,
          regularAction.actionCarrier,
          { regularAction.actionMultiply(l, _) }
        )
        
      morphisms.toSet shouldBe { 
        elementsOf(monoidOf3.carrier).toSet map leftMultiplication
      }

      morphisms.forall { 
        monoidOf3.actions.isMorphism(
          regularAction, 
          regularAction, 
          _
        ) 
      } shouldBe true
    }

    "for the regular action to itself, checked another way" in {
      val wrappedRegularAction =
        actionTopos.makeDot(regularAction)

      canEnumerateMorphisms(
        wrappedRegularAction,
        wrappedRegularAction,
        thorough=false // true passes, but takes too long
      )
    }

    "for a more complex situation" in {
      canEnumerateMorphisms(
        actionTopos.omega.squared,
        actionTopos.omega.squared,
        thorough=false
      )
    }
  }

  def canEnumerateMorphisms[X, Y](
   source: actionTopos.DOT[X],
   target: actionTopos.DOT[Y],
   thorough: Boolean
  ) {
    val sourceAction =
      actionTopos.unwrap(
        source
      )
    val targetAction =
      actionTopos.unwrap(
        target
      )

  val morphisms =
    analysisFor(
      sourceAction
    ).morphismsTo(
      targetAction
    )

    morphisms.forall {
      monoidOf3.actions.isMorphism(
        sourceAction,
        targetAction,
        _
      )
    } shouldBe true

    if (thorough)
      morphisms.toSet map { (morphism: X > Y) =>
        val preArrow =
          new monoidOf3.ActionPreArrow[X, Y](
            sourceAction,
            targetAction,
            x => morphism(x)
          )

        actionTopos.makeArrow(
          preArrow
        )
      } shouldBe {
        (source >> target) toSet
      }
  }

  private def canExtractPresentation[A](
      action: monoidOf3.Action[A]
    ) {
      val generatorsWithRelators: Seq[GeneratorWithRelators[Symbol, A]] =
        analysisFor(
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
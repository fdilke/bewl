package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{ >, ToposOfMonoidActions }
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import FiniteSetsActionAssistant.{ findGenerators, findGeneratorsWithRelators }
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

class FiniteSetsActionAssistantTest extends FreeSpec {
  
    private val (i, x, y) = ('i, 'x, 'y)

    private val monoidOf3 =
      monoidFromTable(
        i, x, y,
        x, x, y,
        y, x, y
      ) // right-dominant on two generators

    private type M = Symbol

    private val actionTopos = 
      ToposOfMonoidActions of monoidOf3
    
    private def canExtractPresentation[A](
      action: monoidOf3.Action[A]
    ) {
      val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
        findGeneratorsWithRelators(
          monoidOf3
        )(
          action
        )

      val presentedAction =
        Presentation(
          monoidOf3
        )(
          generatorsWithRelators
        )
      presentedAction.action.sanityTest

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
    
  "The FiniteSets action assistant" - {
    "can extract a set of generators for a monoid" in {
      import monoidOf3.regularAction

      val generators: M > M =
        findGenerators(
          monoidOf3
        )(
          regularAction
        )
        
      generators shouldBe 'monic

      // Check it's the unique generator
      elementsOf(
        generators.source
      ) shouldBe Seq(i)
    }

    "can extract a presentation for the regular monoid action" in {
      canExtractPresentation(monoidOf3.regularAction)
    }
    "can extract a presentation for an empty monoid action" in {
      canExtractPresentation(        
          actionTopos.unwrap(
            actionTopos.O
          )
        )
    }
    "can extract a presentation for the truth object monoid action" in {
      canExtractPresentation(        
          actionTopos.unwrap(
            actionTopos.omega
          )
        )
    }
    "can extract a presentation for a more fancy monoid action" in {
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
}

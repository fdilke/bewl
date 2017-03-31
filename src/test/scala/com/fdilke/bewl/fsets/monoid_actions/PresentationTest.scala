package com.fdilke.bewl.fsets.monoid_actions;

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.ToposOfMonoidActions
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.language.reflectiveCalls
import scala.language.existentials

import FiniteSets.{>, VOID, ToposOfMonoidActions}

class PresentationTest extends FreeSpec {

  private val (i, x, y) = ('i, 'x, 'y)
  
  private val monoidOf3 =
      monoidFromTable(
        i, x, y,
        x, x, y,
        y, x, y
      ) // right-dominant on two generators

  private val actionTopos = 
      ToposOfMonoidActions of monoidOf3
      
  "Building actions from presentations" - {
		"works for an empty generating set" ignore {
			val presentation = 
			  Presentation(
		      monoidOf3
	      )(
		      List[GeneratorWithRelators[VOID]]()
	      )

      val emptyAction: monoidOf3.Action[actionTopos.VOID] =
        actionTopos.unwrap(
          actionTopos.O
        )	      
	    val emptyProjection: VOID > actionTopos.VOID = 
	      presentation.project(
          emptyAction,
          List[actionTopos.VOID]()
        )
      emptyProjection should have(
        'source(presentation.action),
        'target(emptyAction)
      )
		}
	}
}

package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.>
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import FiniteSetsActionAssistant.generators
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
    
  "The FiniteSets action assistant" - {
    "can extract a set of generators for a monoid" in {
      import monoidOf3.regularAction

      val theGenerators: M > M =
        generators(
          monoidOf3
        )(
          regularAction
        )
        
      theGenerators shouldBe 'monic

      // Check it's the unique generator
      elementsOf(
        theGenerators.source
      ) shouldBe Seq(i)
    }
  }
}
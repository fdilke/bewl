package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.>
import FiniteSetsActionAssistant.extractGenerators

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

      val generators: M > M =
        extractGenerators(
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
  }
}
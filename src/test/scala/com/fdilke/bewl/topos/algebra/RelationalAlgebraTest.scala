package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.Function.untupled
import com.fdilke.bewl.helper.⊕

class RelationalAlgebraTest extends FunSpec {

  describe("Equivalences") {
    it("can be tested for sets") {
      val symbols = dot('A, 'B, 'C)
      val notReflexive = Set(
        'A -> 'A
      )
      val notSymmetric = Set(
        'A -> 'A,
        'B -> 'B,
        'C -> 'C,
        'A -> 'B
      )
      val notTransitive = Set(
        'A -> 'A,
        'B -> 'B,
        'C -> 'C,
        'A -> 'B,
        'B -> 'A,
        'B -> 'C,
        'C -> 'B
      )
      val identifyBandC = Set(
        'A -> 'A,
        'B -> 'B,
        'C -> 'C,
        'B -> 'C,
        'C -> 'B
      )

      symbols.isEquivalenceRelation(
        equivalenceFrom(notReflexive)
      ) shouldBe false

      symbols.isEquivalenceRelation(
        equivalenceFrom(notSymmetric)
      ) shouldBe false

      symbols.isEquivalenceRelation(
        equivalenceFrom(notTransitive)
      ) shouldBe false

      symbols.isEquivalenceRelation(
        equivalenceFrom(identifyBandC)
      ) shouldBe true
    }
  }

}

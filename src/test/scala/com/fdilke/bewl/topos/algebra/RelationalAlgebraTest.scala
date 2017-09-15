package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import Relation.diagonalRelation
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.Function.untupled
import com.fdilke.bewl.helper.⊕

class RelationalAlgebraTest extends FunSpec {

  describe("The diagonal relation") {
    it("is as expected for sets") {
      val carrier = dot(0, 1)
      val diag =
        diagonalRelation(carrier)
      diag(0, 0) shouldBe true
      diag(0, 1) shouldBe false
      diag(1, 0) shouldBe false
    }
  }

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

package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import Relation._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class RelationalAlgebraTest extends FunSpec {

  describe("The diagonal relation") {
    it("is as expected for sets") {
      val carrier = dot(0, 1)
      val diag =
        diagonalRelation(carrier)
      diag(0, 0) shouldBe true
      diag(0, 1) shouldBe false
      diag(1, 0) shouldBe false
      diag(1, 1) shouldBe true

      diag shouldBe relationFrom(
        carrier,
        0 -> 0,
        1 -> 1
      )
    }
  }

  describe("The inverse of a relation") {
    it("is as expected for sets") {
      val carrier = dot(0, 1)

      relationFrom(
        carrier,
        0 -> 0,
        0 -> 1
      ).inverse shouldBe
        relationFrom(
          carrier,
          0 -> 0,
          1 -> 0
        )
    }
  }

  describe("Comparison of relations") {
    it("is as expected for sets") {
      val left = dot(0, 1, 2)
      val right = dot("cuss", "hiss", "silent")

      relationFrom(
        left,
        right,
        0 -> "cuss",
        1 -> "hiss",
        1 -> "silent"
      ) <= relationFrom(
        left,
        right,
        0 -> "cuss",
        1 -> "hiss",
        1 -> "silent",
        2 -> "cuss"
      ) shouldBe true

      relationFrom(
        left,
        right,
        0 -> "cuss",
        1 -> "hiss",
        1 -> "silent"
      ) <= relationFrom(
        left,
        right,
        0 -> "cuss",
        1 -> "silent",
        2 -> "cuss"
      ) shouldBe false

      val diag = diagonalRelation(left)
      diag <= diag shouldBe true
    }
  }

  describe("The composite of two relations") {
    it("is as expected for sets") {
      val left = dot(0, 1, 2)
      val mid = dot(Some(true), Some(false), None)
      val right = dot("cuss", "hiss", "silent")

      relationFrom(
        left,
        mid,
        0 -> Some(true),
        2 -> Some(false)
      ) o relationFrom(
        mid,
        right,
        Some(true) -> "cuss",
        Some(true) -> "hiss",
        Some(false) -> "silent"
      ) shouldBe
        relationFrom(
          left,
          right,
          0 -> "cuss",
          0 -> "hiss",
          2 -> "silent"
        )
    }
  }

  describe("The criterion of reflexivity") {
    it("is as expected for sets") {
      val carrier = dot(0, 1)

      diagonalRelation(
        carrier
      ).isReflexive shouldBe true

      relationFrom(
        carrier,
        0 -> 0,
        1 -> 1,
        0 -> 1
      ).isReflexive shouldBe true
      
      relationFrom(
        carrier,
        0 -> 0,
        0 -> 1
      ).isReflexive shouldBe false
    }
  }

  describe("The criterion of symmetry") {
    it("is as expected for sets") {
      val carrier = dot(0, 1)

      relationFrom(
        carrier,
        0 -> 0,
        0 -> 1
      ).isSymmetric shouldBe false

      relationFrom(
        carrier,
        0 -> 1,
        1 -> 0
      ).isSymmetric shouldBe true

      diagonalRelation(
        carrier
      ).isSymmetric shouldBe true
    }
  }

  describe("The join of relations") {
    it("is as expected for sets") {
      val carrier = dot(0, 1, 2)

      relationFrom(
        carrier,
        0 -> 0,
        0 -> 1
      ) ∨ relationFrom(
        carrier,
        0 -> 1,
        2 -> 1
      ) shouldBe relationFrom(
        carrier,
        0 -> 0,
        0 -> 1,
        2 -> 1
      )
    }
  }

  describe("The criterion of transitivity") {
    it("is as expected for sets") {
      val carrier = dot(0, 1, 2)

      relationFrom(
        carrier
      ).isTransitive shouldBe true

      relationFrom(
        carrier,
        0 -> 1,
        1 -> 2
      ).isTransitive shouldBe false

      relationFrom(
        carrier,
        0 -> 1,
        1 -> 2,
        0 -> 2
      ).isTransitive shouldBe true

      diagonalRelation(
        carrier
      ).isTransitive shouldBe true
    }
  }

  describe("Generated equivalence relations") {
    it("fix the diagonal") {
      val carrier = dot(0, 1, 2)
      val diag =
        diagonalRelation(
          carrier
        )

        diag.toEquivalence shouldBe diag
    }

    it("are as expected for sets") {
      val carrier = dot(0, 1, 2, 3, 4, 5)
      relationFrom(
        carrier,
        1 -> 2,
        2 -> 3,
        3 -> 4
      ).toEquivalence shouldBe
        relationFrom(
          carrier,
          0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5,
          1 -> 2, 2 -> 1,
          2 -> 3, 3 -> 2,
          3 -> 4, 4 -> 3,
          1 -> 3, 3 -> 1,
          1 -> 4, 4 -> 1,
          2 -> 4, 4 -> 2
        )
    }
  }

  describe("The criterion of idempotence") {
    it("is as expected for sets") {
      val carrier = dot(0, 1, 2, 3)

      relationFrom(
        carrier
      ).isIdempotent shouldBe true

      relationFrom(
        carrier,
        0 -> 0,
        0 -> 1,
        1 -> 1,
        1 -> 2
      ).isIdempotent shouldBe false

      relationFrom(
        carrier,
        0 -> 0,
        0 -> 1,
        1 -> 1,
        1 -> 2,
        0 -> 2
      ).isIdempotent shouldBe true

      diagonalRelation(
        carrier
      ).isIdempotent shouldBe true
    }
  }

  describe("Equivalence relations") {
    it("can be tested for sets") {
      val symbols = dot('A, 'B, 'C)
      val notReflexive =
        relationFrom(
          symbols,
          'A -> 'A
        )
      val notSymmetric =
        relationFrom(
          symbols,
          'A -> 'A,
          'B -> 'B,
          'C -> 'C,
          'A -> 'B
        )
      val notTransitive =
        relationFrom(
          symbols,
          'A -> 'A,
          'B -> 'B,
          'C -> 'C,
          'A -> 'B,
          'B -> 'A,
          'B -> 'C,
          'C -> 'B
        )
      val identifyBandC =
        relationFrom(
          symbols,
          'A -> 'A,
          'B -> 'B,
          'C -> 'C,
          'B -> 'C,
          'C -> 'B
        )

      notReflexive.
        isEquivalence shouldBe false

      notSymmetric.
        isEquivalence shouldBe false

      notTransitive.
        isEquivalence shouldBe false

      identifyBandC.
        isEquivalence shouldBe true
    }
  }
}

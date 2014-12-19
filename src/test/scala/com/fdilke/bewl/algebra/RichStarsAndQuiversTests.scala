package com.fdilke.bewl.algebra

import org.scalatest.FunSpec
import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.Matchers._

class RichStarsAndQuiversTests extends FunSpec {

  describe("The universal quantifier") {
    it("detects whether a subobject is the whole object") {
      val totalSet = makeStar(1, 2, 3, 4)
      val subset = makeStar(1, 3)

      val embed = subset(totalSet) { x => x}
      val ∀ = totalSet.∀
      ∀ should have(
        'source(totalSet > omega),
        'target(omega)
      )
      ∀ o (embed.chi.name) should not be(truth)
      ∀ o (totalSet.identity.chi.name) shouldBe truth
    }
  }

  describe("The existential quantifier") {
    it("detects whether a subobject is NOT empty") {
      val totalSet = makeStar(1, 2, 3, 4)
      val subset = makeStar(1, 3)
      val emptySet = makeStar[Int]()

      val embed = subset(totalSet) { x => x}
      val embedEmpty = emptySet(totalSet) { x => x}
      val exists = totalSet.∃
      exists should have(
        'source(totalSet > omega),
        'target(omega)
      )
      exists o (embed.chi.name) should be(truth)
      exists o (embedEmpty.chi.name) should not be(truth)
      exists o (totalSet.identity.chi.name) shouldBe truth
    }
  }

  describe("Sequence comprehensions for operators") {
    it("can define unary operators") {
      val set = makeStar(-1, 0, 1)
      val unaryMinus = for (x <- set) yield -x
      unaryMinus shouldBe makeQuiver(set, set,
        -1 -> 1, 0 -> 0, 1 -> -1
      )
    }

    it("can define binary operators") {
      val three = makeStar(0, 1, 2)
      val subtract = for (x <- three ; y <- three) yield (x - y + 3) % 3;
      subtract shouldBe makeBinaryOperator(three,
        (0, 0) -> 0, (0, 1) -> 2, (0, 2) -> 1,
        (1, 0) -> 1, (1, 1) -> 0, (1, 2) -> 2,
        (2, 0) -> 2, (2, 1) -> 1, (2, 2) -> 0
      )
    }
  }

  describe("The truth object") {
    it("has the correct binary operations for binary operations") {
      TruthObject.and shouldBe (
        for(i <- omega ; j <- omega)
          yield i & j
        )
      TruthObject.implies shouldBe (
        for(i <- omega ; j <- omega)
          yield !i | j
        )
    }
//    subtract shouldBe makeBinaryOperator(set,
//      (0, 0) -> 0, (0, 1) -> 2, (0, 2) -> 1,
//      (1, 0) -> 1, (1, 1) -> 0, (1, 2) -> 2,
//      (2, 0) -> 2, (2, 1) -> 1, (2, 2) -> 0
//    ).quiver
  }
}

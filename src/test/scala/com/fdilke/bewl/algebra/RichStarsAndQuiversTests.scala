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

  describe("Sequence comprehensions for operators") {
    it("can define unary operators") {
      val set = makeStar(-1, 0, 1)
      val unaryMinus = for (x <- set) yield -x
      unaryMinus shouldBe makeQuiver(set, set,
        -1 -> 1, 0 -> 0, 1 -> -1
      )
    }

    it("can define binary operators") {
      val set = makeStar(0, 1, 2)
      val subtract = for (x <- set ; y <- set) yield (x - y + 3) % 3;
      subtract shouldBe makeBinaryOperator(set,
        (0, 0) -> 0, (0, 1) -> 2, (0, 2) -> 1,
        (1, 0) -> 1, (1, 1) -> 0, (1, 2) -> 2,
        (2, 0) -> 2, (2, 1) -> 1, (2, 2) -> 0
      ).quiver
    }
  }
}

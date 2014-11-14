package com.fdilke.bewl.algebra

import org.scalatest.FunSpec
import com.fdilke.bewl.fsets.NativeFiniteSets._
import com.fdilke.bewl.fsets.NativeFiniteSetsUtilities._
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
}

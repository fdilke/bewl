package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicConstructionsTest extends FunSpec {

  private val topos = com.fdilke.bewl.fsets.FiniteSets
  import topos._

  describe("The monoid of endomorphisms can be constructed") {
    it("for the empty set") {
      val endosOf0 = endomorphismMonoid(dot())
      endosOf0.sanityTest
      endosOf0.carrier.globals.size shouldBe 1
      endosOf0 should be('commutative)
    }

    it("for a 1-element set") {
      val endosOf1 = endomorphismMonoid(dot('x))
      endosOf1.sanityTest
      endosOf1.carrier.globals.size shouldBe 1
      endosOf1 should be('commutative)
    }

    it("for a 2-element set") {
      val endosOf2 = endomorphismMonoid(dot('x, 'y))
      endosOf2.sanityTest
      endosOf2.carrier.globals.size shouldBe 4
      endosOf2 should not be('commutative)
    }

    it("for a 3-element set") {
      val endosOf3 = endomorphismMonoid(dot('x, 'y, 'z))
      endosOf3.sanityTest
      endosOf3.carrier.globals.size shouldBe 27
      endosOf3 should not be('commutative)
    }
  }
}

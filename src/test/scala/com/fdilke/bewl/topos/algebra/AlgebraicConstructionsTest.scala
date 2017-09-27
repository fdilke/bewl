package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicConstructionsTest extends FunSpec {

  private val topos = com.fdilke.bewl.fsets.FiniteSets
  import topos._

  describe("The monoid of endomorphisms can be constructed") {
    it("for the empty set") {
      val endosOf0 = endomorphismMonoid(dot()).monoid
      endosOf0 shouldBe an[Monoid[_]]
      endosOf0.sanityTest
      endosOf0.carrier.size shouldBe 1
      endosOf0 shouldBe 'commutative
    }

    it("for a 1-element set") {
      val endosOf1 = endomorphismMonoid(dot('x)).monoid
      endosOf1.sanityTest
      endosOf1.carrier.size shouldBe 1
      endosOf1 shouldBe 'commutative
    }

    it("for a 2-element set") {
      val endosOf2 = endomorphismMonoid(dot('x, 'y)).monoid
      endosOf2.sanityTest
      endosOf2.carrier.size shouldBe 4
      endosOf2 should not be 'commutative
    }

    it("for a 3-element set") {
      val endosOf3 = endomorphismMonoid(dot('x, 'y, 'z)).monoid
      endosOf3.sanityTest
      endosOf3.carrier.size shouldBe 27
      endosOf3 should not be 'commutative
    }

    it("and has a 'home' action on the original object") {
      val three: FiniteSetsDot[Symbol] = dot('x, 'y, 'z)
      val endosOf3 = endomorphismMonoid(three)
      import endosOf3.homeAction
      homeAction.carrier shouldBe three
      for {
        symbol <- elementsOf(three)
        mapping <- elementsOf(endosOf3.monoid.carrier)
      }
        homeAction.actionMultiply(
          symbol,
          mapping
        ) should equal(
          mapping(symbol)
        )
    }
  }

  describe("The group of units for a monoid can be constructed") {
    it("for the trivial monoid") {
      val monoid = monoidFromTable('o)
      val (group, inject) = groupOfUnits(monoid)
      group shouldBe a[Group[_]]
      group.sanityTest
      group.carrier.size shouldBe 1
      inject should have(
        'source(group.carrier),
        'target(monoid.carrier),
        'iso(true)
      )
    }

    it("for a deliberately not very invertible monoid") {
      val monoid = monoidFromTable(
        'o, 'x,
        'x, 'x
      )
      val (group, inject) = groupOfUnits(monoid)
      group.sanityTest
      group.carrier.size shouldBe 1
      group shouldBe 'commutative
      inject should have(
        'source(group.carrier),
        'target(monoid.carrier),
        'monic(true),
        'iso(false)
      )
      monoids.isMorphism(group.asMonoid, monoid, inject) shouldBe true
    }

    it("for a monoid that is a group already") {
      val monoid: FiniteSets.Monoid[Symbol] = monoidFromTable(
        'o, 'x,
        'x, 'o
      )
      val (group, inject) = groupOfUnits(monoid)
      group.sanityTest
      group.carrier.size shouldBe 2
      group shouldBe 'commutative
      inject should have(
        'source(group.carrier),
        'target(monoid.carrier),
        'iso(true)
      )
      monoids.isMorphism(group.asMonoid, monoid, inject) shouldBe true
    }

    it("for a larger endomorphism monoid") {
      val group =
        groupOfUnits(
          endomorphismMonoid(dot(1,2,3)).monoid
        )._1
      group.sanityTest
      group.carrier.size shouldBe 6
      group should not be 'commutative
    }
  }
}

package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import FiniteSetsUtilities.allMaps
import org.scalatest.{FunSpec, Matchers}
import Matchers._

class FiniteSetsUtilitiesTest extends FunSpec {
  describe("allMaps()") {
    it("enumerates all maps between two sets") {
      allMaps(Seq(1, 2), Set("a", "b", "c")).map {
        f => Map(1->f(1), 2->f(2))
      } shouldBe Seq(
        Map(1->"a", 2->"a"), Map(1->"b", 2->"a"), Map(1->"c", 2->"a"),
        Map(1->"a", 2->"b"), Map(1->"b", 2->"b"), Map(1->"c", 2->"b"),
        Map(1->"a", 2->"c"), Map(1->"b", 2->"c"), Map(1->"c", 2->"c")
      )
    }

    it("gives sensible results even when the source is empty") {
      allMaps(Seq(), Seq(0)) should have size 1
    }

    it("gives sensible results even when the target is empty") {
      allMaps(Seq(0), Seq()) shouldBe 'empty
    }

    it("gives sensible results even when both source and target are empty") {
      allMaps(Seq(), Seq()) should have size 1
    }
  }

  describe("The double characteristic") {
    it("turns a family of subsets into an arrow from the powerset object to omega") {
      val numbers = dot(1, 2, 3)

      val doubleChar =
        doubleCharacteristic(
          numbers
        )(
          Set(1, 2),
          Set(3)
        )

      doubleChar should have(
        'source(numbers.power),
        'target(omega)
      )

      for {
        f <- elementsOf(numbers.power)
        x <- elementsOf(numbers)
      }
        doubleChar(f) shouldBe (
          ( f(1) && f(2) && !f(3)) ||
          ( !f(1) && !f(2) && f(3))
        )
    }
  }
}
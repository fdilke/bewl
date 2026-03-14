package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{allMaps, _}
import com.fdilke.bewl.helper.StandardSymbols.{source, target}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class FiniteSetsUtilitiesTest extends AnyFreeSpec {
  "allMaps()" - {
    "enumerates all maps between two sets" in {
      allMaps(Seq(1, 2), Set("a", "b", "c")).map { f =>
        Map(1 -> f(1), 2 -> f(2))
      } shouldBe Seq(
        Map(1 -> "a", 2 -> "a"),
        Map(1 -> "b", 2 -> "a"),
        Map(1 -> "c", 2 -> "a"),
        Map(1 -> "a", 2 -> "b"),
        Map(1 -> "b", 2 -> "b"),
        Map(1 -> "c", 2 -> "b"),
        Map(1 -> "a", 2 -> "c"),
        Map(1 -> "b", 2 -> "c"),
        Map(1 -> "c", 2 -> "c")
      )
    }

    "gives sensible results even when the source is empty" in {
      allMaps(Seq(), Seq(0)).size shouldBe 1
    }

    "gives sensible results even when the target is empty" in {
      allMaps(Seq(0), Seq()) shouldBe empty
    }

    "gives sensible results even when both source and target are empty" in {
      allMaps(Seq(), Seq()).size shouldBe 1
    }
  }

  "The double characteristic" - {
    "turns a family of subsets into an arrow from the powerset object to omega" in {
      val numbers = dot(1, 2, 3)

      val doubleChar =
        doubleCharacteristic(
          numbers
        )(
          Set(1, 2),
          Set(3)
        )

      doubleChar should have(
        source(numbers.power),
        target(omega)
      )

      for {
        f <- elementsOf(numbers.power)
        x <- elementsOf(numbers)
      } doubleChar(f) shouldBe (
        (f(1) && f(2) && !f(3)) ||
          (!f(1) && !f(2) && f(3))
      )
    }
  }
}

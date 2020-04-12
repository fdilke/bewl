package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.fsets.FiniteSetsUtilities.{allMaps, _}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

class FiniteSetsUtilitiesTest extends AnyFreeSpec {
  "allMaps()" - {
    "enumerates all maps between two sets" in {
      allMaps(Iterable(1, 2), Iterable("a", "b", "c")).map { f =>
        Map(1 -> f(1), 2 -> f(2))
      } shouldBe Iterable(
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
      allMaps(Iterable(), Iterable(0)).size shouldBe 1
    }

    "gives sensible results even when the target is empty" in {
      allMaps(Iterable(0), Iterable()) shouldBe empty
    }

    "gives sensible results even when both source and target are empty" in {
      allMaps(Iterable(), Iterable()).size shouldBe 1
    }
  }
}

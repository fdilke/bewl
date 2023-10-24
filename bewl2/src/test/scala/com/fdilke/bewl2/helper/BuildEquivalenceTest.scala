package com.fdilke.bewl.helper

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.{existentials, reflectiveCalls}

class BuildEquivalenceTest extends AnyFreeSpec {
  "Equivalence relations" - {
    "can be calculated over an empty set" in {
      BuildEquivalence(
        0,
        Set.empty
      ) shouldBe List()
    }
    "can be calculated over a singleton with no relators" in {
      BuildEquivalence(
        1,
        Set.empty
      ) shouldBe List(
        0
      )
    }
    "can be calculated over a doubleton with no relators" in {
      BuildEquivalence(
        2,
        Set.empty
      ) shouldBe List(
        0,
        1
      )
    }
    "can be calculated over a doubleton with only trivial relators" in {
      BuildEquivalence(
        2,
        Iterable(
          0 -> 0,
          1 -> 1
        )
      ) shouldBe List(
        0,
        1
      )
    }
    "can be calculated over a doubleton with a relator equating the elements" in {
      BuildEquivalence(
        2,
        Iterable(
          0 -> 1
        )
      ) shouldBe List(
        1,
        1
      )
    }
    "can be calculated for a nontrivial example" in {
      BuildEquivalence(
        4,
        Iterable(
          1 -> 2
        )
      ) shouldBe List(
        0,
        2,
        2,
        3
      )
    }
    "can be calculated for a bigger example" in {
      BuildEquivalence(
        6,
        Iterable(
          1 -> 2,
          2 -> 3
        )
      ) shouldBe List(
        0, 3, 3, 3, 4, 5
      )
    }
    "can be calculated for a yet bigger example" in {
      BuildEquivalence(
        10,
        Iterable(
          1 -> 2,
          7 -> 0,
          4 -> 3,
          3 -> 7,
          6 -> 5,
          9 -> 5
        )
      ) shouldBe List(
        0, 2, 2, 0, 0, 5, 5, 0, 8, 5
      )
    }
    "can be calculated for a formerly problematic example" in {
      BuildEquivalence(
        4,
        Iterable(
          2 -> 0,
          3 -> 1,
          2 -> 2,
          3 -> 3,
          1 -> 3,
          0 -> 2,
          2 -> 2,
          3 -> 3
        )
      ) shouldBe List(
        0,
        1,
        0,
        1
      )
    }
    "can be calculated for another formerly problematic example" in {
      BuildEquivalence(
        4,
        Iterable(
          0 -> 0,
          0 -> 1,
          0 -> 2,
          0 -> 3,
          1 -> 0,
          1 -> 1,
          1 -> 2,
          1 -> 3,
          2 -> 0,
          2 -> 1,
          2 -> 2,
          2 -> 3,
          3 -> 0,
          3 -> 1,
          3 -> 2,
          3 -> 3
        )
      ) shouldBe List(
        3,
        3,
        3,
        3
      )
    }
  }
}

// memo: how to tag:
// taggedAs(new org.scalatest.Tag("MyTag"))
// test-only com.fdilke.bewl.helper.BuildEquivalenceTest -- -n MyTag

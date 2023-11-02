package com.fdilke.bewl.helper

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.helper.BuildEquivalence
import scala.language.{existentials, reflectiveCalls}

class BuildEquivalenceTest extends FunSuite:

  test("Equivalence relations can be calculated over an empty set"):
    BuildEquivalence(
      0,
      Set.empty
    ) is List()

  test("Equivalence relations can be calculated over a singleton with no relators"):
    BuildEquivalence(
      1,
      Set.empty
    ) is List(
      0
    )

  test("Equivalence relations can be calculated over a doubleton with no relators"):
    BuildEquivalence(
      2,
      Set.empty
    ) is List(
      0,
      1
    )

  test("Equivalence relations can be calculated over a doubleton with only trivial relators"):
    BuildEquivalence(
      2,
      Iterable(
        0 -> 0,
        1 -> 1
      )
    ) is List(
      0,
      1
    )

  test("Equivalence relations can be calculated over a doubleton with a relator equating the elements"):
    BuildEquivalence(
      2,
      Iterable(
        0 -> 1
      )
    ) is List(
      1,
      1
    )

  test("Equivalence relations can be calculated for a nontrivial example"):
      BuildEquivalence(
        4,
        Iterable(
          1 -> 2
        )
      ) is List(
        0,
        2,
        2,
        3
      )
    
  test("Equivalence relations can be calculated for a bigger example"):
    BuildEquivalence(
      6,
      Iterable(
        1 -> 2,
        2 -> 3
      )
    ) is List(
      0, 3, 3, 3, 4, 5
    )

  test("Equivalence relations can be calculated for a yet bigger example"):
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
    ) is List(
      0, 2, 2, 0, 0, 5, 5, 0, 8, 5
    )

  test("Equivalence relations can be calculated for a formerly problematic example"):
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
    ) is List(
      0,
      1,
      0,
      1
    )

  test("Equivalence relations can be calculated for another formerly problematic example"):
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
    ) is List(
      3,
      3,
      3,
      3
    )

// memo: how to tag:
// taggedAs(new org.scalatest.Tag("MyTag"))
// test-only com.fdilke.bewl.helper.BuildEquivalenceTest -- -n MyTag

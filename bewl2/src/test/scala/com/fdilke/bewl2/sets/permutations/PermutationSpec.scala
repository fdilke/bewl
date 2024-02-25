package com.fdilke.bewl2.sets.permutations

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import Permutation._

class PermutationSpec extends FunSuite:
  test("Permutation can permute distinct objects in a cycle, and knows its domain"):
    val permutation: Permutation[Int] =
      Permutation(0, 1)
    permutation.domain is Set[Int](0, 1)

  test("Permutation can operate on objects in its domain, no others"):
    val permutation: Permutation[Int] =
      Permutation(0, 1)
    permutation(0) is 1
    permutation(1) is 0
    intercept[IllegalArgumentException]:
      permutation(2)

  test("Permutation can include multiple cycles of different lengths"):
    val permutation: Permutation[Int] =
      Permutation(0, 1)(2, 3, 4)
    permutation(0) is 1
    permutation(1) is 0
    permutation(2) is 3
    permutation(3) is 4
    permutation(4) is 2
    permutation.domain is Set[Int](0, 1, 2, 3, 4)
    intercept[IllegalArgumentException]:
      permutation(5)

  test("Permutation must permute distinct objects in a cycle"):
    intercept[IllegalArgumentException]:
      Permutation(1, 1)

// tests for equality, with awkward edge cases


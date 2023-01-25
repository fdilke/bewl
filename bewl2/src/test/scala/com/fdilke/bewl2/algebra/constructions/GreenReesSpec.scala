package com.fdilke.bewl2.algebra.constructions
import GreenRees.*
import com.fdilke.bewl2.utility.RichFunSuite

class GreenReesSpec extends RichFunSuite:

  test("Can extract left segments from a word") {
    intercept[java.lang.IllegalArgumentException]{
      extractLeftSegment(Seq())
    }
    extractLeftSegment(Seq(0)) is (Seq(), 0)
    extractLeftSegment(Seq(0, 1)) is (Seq(0), 1)
    extractLeftSegment(Seq(0, 1, 2)) is (Seq(0, 1), 2)
    extractLeftSegment(Seq(2, 0, 0, 2, 1, 0, 2)) is (Seq(2, 0, 0, 2), 1)
  }

  test("Can factorize a word") {
    intercept[java.lang.IllegalArgumentException]{
      factorize[Int](Seq())
    }
    factorize[Int](Seq(0)) is Factorization(Seq(), 0, 0, Seq())
    factorize[Int](Seq(0, 1, 2)) is Factorization(Seq(0, 1), 2, 0, Seq(1, 2))
    factorize[Int](Seq(0, 1, 1, 2)) is Factorization(Seq(0, 1, 1), 2, 0, Seq(1, 1, 2))
    factorize[Int](Seq(2, 1, 1, 0, 0, 2, 1, 2)) is Factorization(Seq(2, 1, 1), 0, 0, Seq(2, 1, 2))
  }


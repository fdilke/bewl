package com.fdilke.bewl2.algebra.constructions
import GreenRees.*
import com.fdilke.bewl2.utility.RichFunSuite

class GreenReesSpec extends RichFunSuite:

  test("Can extract left segments from a word") {
    intercept[java.lang.IllegalArgumentException]{
      Seq[Int]().leftSegment
    }
    Seq(0).leftSegment is (Seq(), 0)
    Seq(0, 1).leftSegment is (Seq(0), 1)
    Seq(0, 1, 2).leftSegment is (Seq(0, 1), 2)
    Seq(2, 0, 0, 2, 1, 0, 2).leftSegment is (Seq(2, 0, 0, 2), 1)
  }

  test("Can factorize a word") {
    intercept[java.lang.IllegalArgumentException]{
      Seq[Int]().factorize
    }
    Seq(0).factorize is Factorization(Seq(), 0, 0, Seq())
    Seq(0, 1, 2).factorize is Factorization(Seq(0, 1), 2, 0, Seq(1, 2))
    Seq(0, 1, 1, 2).factorize is Factorization(Seq(0, 1, 1), 2, 0, Seq(1, 1, 2))
    Seq(2, 1, 1, 0, 0, 2, 1, 2).factorize is Factorization(Seq(2, 1, 1), 0, 0, Seq(2, 1, 2))
  }


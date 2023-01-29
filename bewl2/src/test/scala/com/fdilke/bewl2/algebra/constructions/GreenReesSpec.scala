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

  test("Can use strings interchangeably with Seq[Char]") {
    intercept[java.lang.IllegalArgumentException]{
      Seq[Int]().factorize
    }
    "a".factorize is Factorization("", 'a', 'a', Seq())
    "abc".factorize is Factorization("ab", 'c', 'a', "bc")
    "abbc".factorize is Factorization("abb", 'c', 'a', "bbc")
    "cbbaacbc".factorize is Factorization("cbb", 'a', 'a', "cbc")
  }

  test("Can check words for equivalence") {
    ("" =!= "") is true
    ("x" =!= "") is false
    ("" =!= "x") is false
    ("ab" =!= "bc") is false
    ("abc" =!= "abc") is true
    // and the interesting cases
    ("bacabc" =!= "bacbcabc") is true
    ("bacabc" =!= "babcabc") is false
    ("xzyzxzy" =!= "xzy") is true
  }

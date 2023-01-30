package com.fdilke.bewl2.algebra.constructions
import GreenRees.*
import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*

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
  
  test("Can optimally 'recombine' a factorization") {
    Factorization("", 'a', 'a', "").recombine be "a"
    Factorization("", 'a', 'b', "").recombine be "ab"
    Factorization("b", 'a', 'b', "a").recombine be "ba"
    Factorization("bac", 'd', 'b', "acd").recombine be "bacd"
    Factorization("bac", 'd', 'a', "cdb").recombine be "bacdb"
    Factorization("bac", 'd', 'c', "dba").recombine be "bacdba"
    Factorization("bac", 'd', 'd', "bac").recombine be "bacdbac"
    Factorization("cbb", 'a', 'a', "cbc").recombine be "cbbacbc"
    Factorization("cbb", 'a', 'c', "ba").recombine be "cbbacba"
    Factorization("cbb", 'a', 'c', "ba").recombine be "cbbacba"
  }

  test("Canonical form for words") {
    "".canonical be ""
    "a".canonical be "a"
    "abc".canonical be "abc"
    "aa".canonical be "a"
    "abab".canonical be "ab"
    "abcabc".canonical be "abc"
    "gababg".canonical be "gabg"
    "gabcabcgabc".canonical be "gabc"
    "bacbcabc".canonical be "bacabc"
  }

  test("Higher level tests for canonical forms") {
    for {
      length <- (0 to 6).toSeq
      seq <- sequencesOfLength("abc", length)
    } {
      val canon: Seq[Char] = seq.canonical
      (canon.length <= seq.length) is true
      (canon =!= seq) is true
      (canon.canonical == canon) is true
    }
  }

  test("Can multiply words with automatic reduction to canonical form") {
    ("" * "") be ""
    ("a" * "") be "a"
    ("" * "a") be "a"
    ("a" * "a") be "a"
    ("a" * "b") be "ab"
    ("a" * "ab") be "ab"
    ("ab" * "ab") be "ab"
    ("ab" * "ab") be "ab"
    ("bacb" * "cabc") be "bacabc"
  }

  if (false)
    test("Can generate the free idempotent monoid as a raw set of canonicals") {
      for {
        (numLetters, expectedSize) <- Seq(0 -> 1, 1 -> 2, 2 -> 7, 3 -> 160)
      } {
        val letters = "abcde".slice(0, numLetters)
        val rawMonoid: Set[Seq[Char]] = canonicalWords(letters)
        rawMonoid.size is expectedSize
        for {
          x <- rawMonoid
        } {
          (x * x) is x
          (x * "") is x
          ("" * x) is x
        }
        for {
          x <- rawMonoid
          y <- rawMonoid
          z <- rawMonoid
        } {
          ((x * y) * z) is (x * (y * z))
        }
      }
    }

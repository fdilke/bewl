package com.fdilke.bewl2.algebra.constructions


import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*
import java.util.concurrent.atomic.AtomicBoolean
import com.fdilke.utility.Shortcuts._
import GreenRees.*

class GreenReesSpec extends RichFunSuite:

  test("Can extract left segments from a word") {
    intercept[java.lang.IllegalArgumentException]{
      "".leftSegment
    }
    "a".leftSegment is ("", 'a')
    "ab".leftSegment is ("a", 'b')
    "abc".leftSegment is ("ab", 'c')
    "caacbac".leftSegment is ("caac", 'b')
    "abacab".leftSegment is ("aba", 'c')
    "bcnofne".leftSegment is ("bcnofn", 'e')
    "daguerrotype".leftSegment is ("daguerroty", 'p')
  }

  test("Can factorize a word") {
    Factorization("") is Factorization(None)
    Factorization("a") is Factorization(Some(
      Prefix(Factorization(None), 'a'), 
      Suffix('a', Factorization(None))
    ))
    Factorization("ab") is Factorization(Some(
      Prefix(Factorization("a"), 'b'), 
      Suffix('a', Factorization("b"))
    ))
    Factorization("abc") is Factorization(Some(
      Prefix(Factorization("ab"), 'c'), 
      Suffix('a', Factorization("bc"))
    ))
    Factorization("aba") is Factorization(Some(
      Prefix(Factorization("a"), 'b'), 
      Suffix('b', Factorization("a"))
    ))
    Factorization("caacbac") is Factorization(Some(
      Prefix(Factorization("caac"), 'b'), 
      Suffix('b', Factorization("ac"))
    ))
    Factorization("abacab") is Factorization(Some(
      Prefix(Factorization("aba"), 'c'), 
      Suffix('c', Factorization("ab"))
    ))
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

  test("Can calculate 'sandwiched least common multiple' of two words") {
    sandwich("", "") is ""
    sandwich("a", "") is "a"
    sandwich("a", "a") is "a"
    sandwich("ab", "b") is "ab"
    sandwich("ab", "a") is "aba"
    sandwich("ba", "ba") is "ba"
    sandwich("bac", "acd") is "bacd"
    sandwich("bac", "cdb") is "bacdb"
    sandwich("cbba", "bba") is "cbba"
    sandwich("cbba", "cba") is "cbbacba"
  }

  test("compute canonical form for words") {
      "".canonical is ""
      "a".canonical is "a"
      "abc".canonical is "abc"
      "aa".canonical is "a"
      "aab".canonical is "ab"
      "baa".canonical is "ba"
      "baab".canonical is "bab"
      "abab".canonical is "ab"
      "abcabc".canonical is "abc"
      "gababg".canonical is "gabg"
      "gabcabcgabc".canonical is "gabc"
      "bacbcabc".canonical is "bacabc"
  }

  test("Canonical form function is idempotent, non-length-increasing") {
    for {
      length <- (0 to 6).toSeq
      word <- wordsOfLength("abc", length)
    } {
      val canon: String = word.canonical
      (canon.length <= word.length) is true
      (canon =!= word) is true
      (canon.canonical == canon) is true
    }
  }

  test("Can multiply words with automatic reduction to canonical form") {
    ("" * "") is  ""
    ("a" * "") is  "a"
    ("" * "a") is  "a"
    ("a" * "a") is  "a"
    ("a" * "b") is  "ab"
    ("a" * "ab") is  "ab"
    ("ab" * "ab") is  "ab"
    ("ab" * "ab") is  "ab"
    ("bacb" * "cabc") is  "bacabc"
  }

  test("Calculate orders of free monoids") {
    val expectedSizes: Seq[BigInt] =
      Seq(1, 2, 7, 160, 332381).map { BigInt(_) } ++ Seq(BigInt("2751884514766"))
    for { i <- expectedSizes.indices } {
      orderFree(i) is expectedSizes(i)
    }
  }
    
  test("Enumerate canonical forms in a free monoid") {
    for { numLetters <- 0 to 2 } {
      val expectedSize = orderFree(numLetters).toInt
      val letters: String = "abcde".slice(0, numLetters)
      val monoid: Set[String] = setOfCanonicals(letters)
      monoid.size is expectedSize
      for {
        x <- monoid
      } {
        (x * x) is x
        (x * "") is x
        ("" * x) is x
      }
      for {
        x <- monoid
        y <- monoid
      } {
        (monoid contains (x * y)) is true
      }
      for {
        x <- monoid
        y <- monoid
        z <- monoid
      } {
        ((x * y) * z) is (x * (y * z))
      }
    }
  }
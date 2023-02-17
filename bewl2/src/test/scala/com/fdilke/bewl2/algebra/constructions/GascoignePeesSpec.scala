package com.fdilke.bewl2.algebra.constructions
import GascoignePees.*
import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*
import java.util.concurrent.atomic.AtomicBoolean
import com.fdilke.utility.Shortcuts._
import com.fdilke.bewl2.algebra.constructions.Factoring

class GascoignePeesSpec extends RichFunSuite:

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
    Seq(0).factorize is Factoring(Seq(), 0, 0, Seq())
    Seq(0, 1, 2).factorize is Factoring(Seq(0, 1), 2, 0, Seq(1, 2))
    Seq(0, 1, 1, 2).factorize is Factoring(Seq(0, 1, 1), 2, 0, Seq(1, 1, 2))
    Seq(2, 1, 1, 0, 0, 2, 1, 2).factorize is Factoring(Seq(2, 1, 1), 0, 0, Seq(2, 1, 2))
  }

  test("Can use strings interchangeably with Seq[Char]") {
    intercept[java.lang.IllegalArgumentException]{
      Seq[Int]().factorize
    }
    "a".factorize is Factoring("", 'a', 'a', Seq())
    "abc".factorize is Factoring("ab", 'c', 'a', "bc")
    "abbc".factorize is Factoring("abb", 'c', 'a', "bbc")
    "cbbaacbc".factorize is Factoring("cbb", 'a', 'a', "cbc")
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
    Factoring("", 'a', 'a', "").recombine be "a"
    Factoring("", 'a', 'b', "").recombine be "ab"
    Factoring("b", 'a', 'b', "a").recombine be "ba"
    Factoring("bac", 'd', 'b', "acd").recombine be "bacd"
    Factoring("bac", 'd', 'a', "cdb").recombine be "bacdb"
    Factoring("bac", 'd', 'c', "dba").recombine be "bacdba"
    Factoring("bac", 'd', 'd', "bac").recombine be "bacdbac"
    Factoring("cbb", 'a', 'a', "cbc").recombine be "cbbacbc"
    Factoring("cbb", 'a', 'c', "ba").recombine be "cbbacba"
    Factoring("cbb", 'a', 'c', "ba").recombine be "cbbacba"
  }

  if (false) // TODO: fix
    test("The 'slow' (i.e. more theoretically grounded) canonical form for words") {
      // "".slowcan be ""
      // "a".slowcan be "a"
      "abc".slowcan be "abc"
      // "aa".slowcan be "a"
      // "aab".slowcan be "ab"
      // "baa".slowcan be "ba"
      // "baab".slowcan be "bab"
      // "abab".slowcan be "ab"
      // "abcabc".slowcan be "abc"
      // "gababg".slowcan be "gabg"
      // "gabcabcgabc".slowcan be "gabc"
      // "bacbcabc".slowcan be "bacabc"
    }

  test("Canonical form for words") {
    "".canonical be ""
    "a".canonical be "a"
    "abc".canonical be "abc"
    "aa".canonical be "a"
    "aab".canonical be "ab"
    "baa".canonical be "ba"
    "baab".canonical be "bab"
    "abab".canonical be "ab"
    "abcabc".canonical be "abc"
    "gababg".canonical be "gabg"
    "gabcabcgabc".canonical be "gabc"
    "bacbcabc".canonical be "bacabc"
  }

  test("Canonical form function is idempotent, non-length-increasing") {
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

  // TODO: fix
  // test("Canonical form is lex-minimal word of the same length") {
  //   for {
  //     length <- (0 to 6).toSeq
  //     seq <- sequencesOfLength("abc", length)
  //   } {
  //     val canon: Seq[Char] = seq.canonical
  //     for { seq2 <- sequencesOfLength("abc", length) } {
  //       if (seq =!= seq2) {
  //         (canon.string <= seq.string) is true
  //       }
  //     }
  //   }
  // }

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

  test("Calculate orders of free monoids") {
    val expectedSizes: Seq[BigInt] =
      Seq(1, 2, 7, 160, 332381).map { BigInt(_) } ++ Seq(BigInt("2751884514766"))
    for { i <- expectedSizes.indices } {
      orderFree(i) is expectedSizes(i)
    }
  }

  private def testEnumerateWords(
    canonicalsFunc: Seq[Char] => Set[Seq[Char]]
  ): Unit =
    for { numLetters <- 0 to 3 } {
      val expectedSize = orderFree(numLetters).toInt
      val letters = "abcde".slice(0, numLetters)
      val rawMonoid: Set[Seq[Char]] = canonicalsFunc(letters)
      rawMonoid.size is expectedSize
      for {
        x <- rawMonoid
      } {
        (x * x) is x
        (x * "") is x
        ("" * x) is x
      }
      if (numLetters < 3)
        for {
          x <- rawMonoid
          y <- rawMonoid
          z <- rawMonoid
        } {
          ((x * y) * z) is (x * (y * z))
        }
    }
      
  test("Generate the free idempotent monoid as a raw set, by squares") {
    testEnumerateWords(enumerateWordsBySquares[Char])
  }

  test("Generate the free idempotent monoid as a raw set, by letters") {
    testEnumerateWords(enumerateWordsByLetters[Char])
  }

  test("Generate the free idempotent monoid as a raw set, by tree") {
    testEnumerateWords(enumerateWordsByTree[Char])
  }

  // test("More tests of the tree") {
  //   enumerateWordsByTree("abcd").size is 332381
  // }
    
  test("Check the longest canonical word on each alphabet") {
    val expected: Seq[Int] = Seq(0, 1, 3, 8, 18)
    for (n <- 0 to 3) {
      val letters: String = "abcde".slice(0, n)
      val longest: Int =
        enumerateWordsByTree(letters).map { _.length }.max
      longest is expected(n)
    }
  }

  test("calculation of the length of the longest canonical word") {
    val expected: Seq[Int] = Seq(0, 1, 3, 8, 18, 38)
    for (n <- expected.indices) {
      longestLength(n) is expected(n)
    }
  }

  test("calculation of a longest canonical word") {
    val expected: Seq[String] =
      Seq("", "a", "aba", "abacabcb", "abacabcbdabdbcbdcd" 
        // "abacabcbdabdbcbdcdeabdbcbdcdebdedcdece"
        // TODO: fix
      )

    for (n <- expected.indices) {
      val letters: String = "abcde".slice(0, n)
      val word: String = longestWord(letters).string
      // println(s"word = '$word' expected(n) = '${expected(n)}'")
      word is expected(n)
      word.length() is longestLength(n)
      word.canonical.string is word
    }
    // also each longest word is a prefix of the next one:
    for (n <- expected.indices.tail) {
      val prevWord = expected(n-1)
      val word = expected(n)
      word.startsWith(prevWord) is true
    }
  }

  test("calculate the Green ordering") {
    ("" <~ "") is true
    ("" <~ "a") is true
    ("a" <~ "b") is false
    ("ab" <~ "b") is false
    ("ab" <~ "ab") is true
    ("ab" <~ "aba") is true
    ("aba" <~ "ab") is true
    ("caba" <~ "cab") is true
  }

  test("calculate the Green R equivalence") {
    ("" R "") is true
    ("" R "a") is false
    ("a" R "b") is false
    ("ab" R "b") is false
    ("ab" R "ab") is true
    ("ab" R "aba") is true
    ("aba" R "ab") is true
    ("caba" R "cab") is true
  }


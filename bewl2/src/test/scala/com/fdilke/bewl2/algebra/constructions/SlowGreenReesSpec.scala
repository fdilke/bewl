package com.fdilke.bewl2.algebra.constructions

import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*
import java.util.concurrent.atomic.AtomicBoolean
import com.fdilke.utility.Shortcuts._
import GreenRees.*

abstract class SlowGreenReesSpec extends RichFunSuite:
  
  test("Canonical form is shortest equivalent word") {
    for {
      length <- (0 to 6).toSeq
      word <- wordsOfLength("abc", length)
    } {
      val canon: String = word.canonical
      for { word2 <- wordsOfLength("abc", length) } {
        if (word =!= word2) 
          ( (canon == word2) || 
            (canon.length < word2.length)
          ) is true
      }
    }
  }

  test("Enumerate canonical forms in a free monoid") {
    for { numLetters <- 0 to 3 } {
      val expectedSize = orderFree(numLetters).toInt
      val letters: String = "abcde".slice(0, numLetters)
      val monoid: Set[String] = enumerateCanonicals(letters).toSet
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
      if (numLetters < 3)
        for {
          x <- monoid
          y <- monoid
          z <- monoid
        } {
          ((x * y) * z) is (x * (y * z))
        }
    }
  }

  test("calculation of a longest canonical word") {
    val expected: Seq[String] =
      Seq("", "a", "aba", "abacabcb", "abacabcbdabdbcbdcd",
        "abacabcbdabdbcbdcdeabcbdbcdcebcecdcede"
        // "abacabcbdabdbcbdcdeabcbdbcdcebcecdcedefabcbdbcdcfbcfcdcfdfebcdcecdedfcdfdedfef"
      )

    for (n <- expected.indices) {
      val word: String = longestWord(n)
      word is expected(n)
      word.length() is longestLength(n)
      word.isCanonical is true

      if (n <= 3) {
        val canonicals: Iterable[String] =
          enumerateCanonicals(alphabetOfSize(n))
        val longest: Int =
          canonicals.map { _.length }.max
        longest is expected(n).length()
        canonicals.filter { _.length == longest }.min is expected(n)
      }
    }

    // also each longest word is a prefix of the next one:
    for (n <- expected.indices.tail) {
      val prevWord = expected(n-1)
      val word = expected(n)
      word.startsWith(prevWord) is true
    }
  }

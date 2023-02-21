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
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
    "".factorize is None
    "a".factorize is Some(Factorization(None, 'a', 'a', None))
    "ab".factorize is Some(Factorization("a", 'b', 'a', "b"))
    "abc".factorize is Some(Factorization("ab", 'c', 'a', "bc"))
    "aba".factorize is Some(Factorization("a", 'b', 'b', "a"))
    "caacbac".factorize is Some(Factorization("caac", 'b', 'b', "ac"))
    "abacab".factorize is Some(Factorization("aba", 'c', 'c', "ab"))
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


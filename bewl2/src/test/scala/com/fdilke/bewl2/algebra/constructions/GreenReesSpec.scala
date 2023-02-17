package com.fdilke.bewl2.algebra.constructions


import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*
import java.util.concurrent.atomic.AtomicBoolean
import com.fdilke.utility.Shortcuts._
import com.fdilke.bewl2.algebra.constructions.Factoring
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
    "a".factorize is Some(Factorization("", 'a', 'a', ""))
    "ab".factorize is Some(Factorization("a", 'b', 'a', "b"))
    "abc".factorize is Some(Factorization("ab", 'c', 'a', "bc"))
    "caacbac".factorize is Some(Factorization("caac", 'b', 'b', "ac"))
    "abacab".factorize is Some(Factorization("aba", 'c', 'c', "ab"))
  }

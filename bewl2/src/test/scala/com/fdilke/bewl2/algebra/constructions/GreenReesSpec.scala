package com.fdilke.bewl2.algebra.constructions


import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*
import java.util.concurrent.atomic.AtomicBoolean
import com.fdilke.utility.Shortcuts._
import com.fdilke.bewl2.algebra.constructions.Factorization
import GreenRees.*

class GreenReesSpec extends RichFunSuite:

  test("Can extract left segments from a word") {
    intercept[java.lang.IllegalArgumentException]{
      "".leftSegment
    }
    "a".leftSegment is "a"
    "ab".leftSegment is "ab"
    "abc".leftSegment is "abc"
    "caacbac".leftSegment is "caacb"
    "abacab".leftSegment is "abac"
    "bcnofne".leftSegment is "bcnofne"
    "daguerrotype".leftSegment is "daguerrotyp"
  }

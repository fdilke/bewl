package com.fdilke.bewl2.utility

import munit.FunSuite
import com.fdilke.bewl2.sets.Sets

trait RichFunSuite extends FunSuite:
  extension[A] (a: A)
    inline def is(b: A): Unit =
      assertEquals(a, b)

  extension(letters: Seq[Char])
    inline def is(text: String): Unit =
      assertEquals(letters, text.toSeq)
      
  import Sets._

  implicit class arrowComparisons[A: Dot, B: Dot](arrow: A => B):
    inline def isArrow(arrow2: A => B): Unit =
      assert(arrow =!= arrow2)
    inline def isNotArrow(arrow2: A => B): Unit =
      assert(!(arrow =!= arrow2))


package com.fdilke.bewl2.utility

import munit.FunSuite
import com.fdilke.bewl2.sets.Sets

trait RichFunSuite extends FunSuite:
  extension[A] (a: A)
    inline def is(b: A): Unit =
      assertEquals(a, b)

  import Sets._

  extension[A: Set, B: Set] (arrow: A => B)
    inline def isArrow(arrow2: A => B): Unit =
      assert(arrow =!= arrow2)
    inline def isNotArrow(arrow2: A => B): Unit =
      assert(!(arrow =!= arrow2))


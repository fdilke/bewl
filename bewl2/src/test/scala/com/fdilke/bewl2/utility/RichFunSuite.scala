package com.fdilke.bewl2.utility

import munit.{ FunSuite, Assertions }
import Assertions.{ assertEquals, assertNotEquals }
import com.fdilke.bewl2.sets.SetsWithSlowActions
import com.fdilke.utility.Shortcuts._

object RichFunSuite:
  extension[A] (a: A)
    infix inline def is(b: A): Unit =
      assertEquals(a, b)
    infix inline def isnt(b: A): Unit =
      assertNotEquals(a, b)

  extension(letters: Seq[Char])
    inline def be(text: String): Unit =
      assertEquals(letters.string, text)

  import SetsWithSlowActions._

  implicit class arrowComparisons[A: Dot, B: Dot](arrow: A => B):
    infix inline def isArrow(arrow2: A => B): Unit =
      assert(arrow =!= arrow2)
    infix inline def isNotArrow(arrow2: A => B): Unit =
      assert(!(arrow =!= arrow2))

  def checkSameElementsAs[X](thing: Seq[X], other: Seq[X]): Unit =
    thing.toSet is other.toSet
  
  
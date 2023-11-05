package com.fdilke.bewl2.utility

import munit.{ FunSuite, Assertions }
import com.fdilke.bewl2.sets.Sets
import com.fdilke.utility.Shortcuts._

object RichFunSuite:
  extension[A] (a: A)
    inline def is(b: A): Unit =
      Assertions.assertEquals(a, b)

  extension(letters: Seq[Char])
    inline def be(text: String): Unit =
      assertEquals(letters.string, text)

  import Sets._

  implicit class arrowComparisons[A: Dot, B: Dot](arrow: A => B):
    inline def isArrow(arrow2: A => B): Unit =
      assert(arrow =!= arrow2)
    inline def isNotArrow(arrow2: A => B): Unit =
      assert(!(arrow =!= arrow2))

  def checkSameElementsAs[X](thing: Seq[S], other: Seq[S]): Unit =
    thing.toSet is other.toSet
  // extension(text: String)
  //   inline def is(letters: Seq[Char]): Unit =
  //     assertEquals(letters, text.toSeq)



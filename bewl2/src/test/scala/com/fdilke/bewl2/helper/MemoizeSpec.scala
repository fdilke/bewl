package com.fdilke.bewl2.helper

import munit.FunSuite
import munit.Clue.generate

class MemoizeSpec extends FunSuite:

  private def composite[X, Y](
    setXsetY: (Set[X], Set[Y])
  ): Set[X | Y] = {
    val (setX, setY) = setXsetY

    (setX map { x => x : (X | Y) } ) union (
      setY map { y => y : (X | Y) }
    )
  }

//  private val memoizedFunction =
//    Memoized

  test("sanity of the test") {
    assertEquals(
      composite[Int, String](
        Set(1,2,3),
        Set("hello", "goodbye")
      ),
      Set[Int | String](
        1, 2, 3, "hello", "goodbye"
      )
    )
  }

//  test("can conveniently cache some composite operation") {
//    assert(
//
//    }

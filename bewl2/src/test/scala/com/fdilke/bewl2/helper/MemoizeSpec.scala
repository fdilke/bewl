package com.fdilke.bewl2.helper

import munit.FunSuite
import munit.Clue.generate

class MemoizeSpec extends FunSuite:

  def composite[X, Y](
    setXsetY: (Set[X], Set[Y])
  ): Set[X | Y] = {
    val (setX, setY) = setXsetY

    setX map { x =>
      x : (X | Y)
    } union (
      setY map { y =>
        y : (X | Y)
      }
    )
  }

  private val memoizedFunction:
    [X, Y] => ((Set[X], Set[Y])) => Set[X | Y]
    = Memoize[
      [X, Y] =>> (Set[X], Set[Y]),
      [X, Y] =>> Set[X | Y]
    ](
      [X, Y] => (sets: (Set[X], Set[Y])) => composite[X, Y](sets)
    )

  test("memoized function acts as pass through") {
    val input =
      Set(1,2,3) -> Set("hello", "goodbye")
    val output =
      Set[Int | String](
        1, 2, 3, "hello", "goodbye"
      )
    assertEquals(
      composite[Int, String](
        input
      ),
      output
    )
    assertEquals(
      memoizedFunction[Int, String](
        input
      ),
      output
    )
  }

//  test("can conveniently cache some composite operation") {
//    assert(
//
//    }

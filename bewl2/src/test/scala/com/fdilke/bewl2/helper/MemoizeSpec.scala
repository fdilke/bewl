package com.fdilke.bewl2.helper

import munit.FunSuite
import munit.Clue.generate

import java.util.concurrent.atomic.AtomicInteger

class MemoizeSpec extends FunSuite:

  private val callCount: AtomicInteger =
    AtomicInteger(0)

  private def vanillaFn(text: String): Int =
    callCount.incrementAndGet()
    text.length

  private val memoizedVanilla: String => Int =
    Memoize.vanilla(vanillaFn)

  private def composite[X, Y](
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

  private val memoizedComposite:
    [X, Y] => ((Set[X], Set[Y])) => Set[X | Y]
    = Memoize[
      [X, Y] =>> (Set[X], Set[Y]),
      [X, Y] =>> Set[X | Y]
    ](
      [X, Y] => (sets: (Set[X], Set[Y])) => composite[X, Y](sets)
    )

  test("memoized vanilla function acts as pass through,is cached") {
    assertEquals(
      callCount.get(),
      0
    )
    assertEquals(
      memoizedVanilla("hello"),
      5
    )
    assertEquals(
      callCount.get(),
      1
    )
    assertEquals(
      memoizedVanilla("hello"),
      5
    )
    assertEquals(
      callCount.get(),
      1
    )
  }

  test("memoized function with 2 type args acts as pass through") {
    val input =
      Set(1,2,3) -> Set("hello", "goodbye")
    val output =
      Set[Int | String](
        1, 2, 3, "hello", "goodbye"
      )
    assertEquals(
      memoizedComposite[Int, String](
        input
      ),
      composite[Int, String](
        input
      )
    )
  }

//  test("can conveniently cache some composite operation") {
//    assert(
//
//    }

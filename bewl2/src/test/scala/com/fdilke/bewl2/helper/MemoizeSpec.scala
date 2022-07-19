package com.fdilke.bewl2.helper

import munit.FunSuite
import munit.Clue.generate

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

class MemoizeSpec extends FunSuite:

  class LocalScope {
    val callCount: AtomicInteger =
      AtomicInteger(0)

    def callCountIs(expected: Int): Unit =
      assertEquals(
        callCount.get(),
        expected
      )

    def vanillaFn(text: String): Int =
      callCount.incrementAndGet()
      text.length

    val memoizedVanilla: String => Int =
      Memoize.vanilla(vanillaFn)

    def composite[X, Y](
      setXsetY: (Set[X], Set[Y])
    ): Set[X | Y] =
      callCount.incrementAndGet()
      val (setX, setY) = setXsetY

      setX map { x =>
        x : (X | Y)
      } union (
      setY map { y =>
          y : (X | Y)
        }
    )

    val memoizedComposite:
        [X, Y] => ((Set[X], Set[Y])) => Set[X | Y]
      = Memoize[
        [X, Y] =>> (Set[X], Set[Y]),
        [X, Y] =>> Set[X | Y]
    ](
        [X, Y] => (sets: (Set[X], Set[Y])) => composite[X, Y](sets)
    )
  }

  private val fixture: FunFixture[LocalScope] =
    FunFixture[LocalScope](
      setup = { test =>
        new LocalScope
      },
      teardown = { scope => () }
    )

  fixture.test("memoized vanilla function acts as pass through,is cached") { scope =>
    import scope._

    def confirmVanilla =
      assertEquals(
        memoizedVanilla("hello"),
        5
      )

    callCountIs(0)
    confirmVanilla
    callCountIs(1)
    confirmVanilla
    callCountIs(1)
  }

  fixture.test("memoized function with 2 type args acts as pass through") { scope =>
    import scope._

    val input =
      Set(1,2,3) -> Set("hello", "goodbye")
    val output =
      Set[Int | String](
        1, 2, 3, "hello", "goodbye"
      )

    def confirmComposite =
      assertEquals(
        memoizedComposite[Int, String](
          input
        ),
        output
      )

    callCountIs(0)
    confirmComposite
    callCountIs(1)
    confirmComposite
    callCountIs(1)
  }


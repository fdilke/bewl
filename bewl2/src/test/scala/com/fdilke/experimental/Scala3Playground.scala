package com.fdilke.experimental

import munit.FunSuite

import scala.concurrent.Future

class Scala3Playground extends FunSuite:
  test("kicking the tyres") {
    def whiffle[T](t: T): Boolean = false
    val theSniff: [T] => T => Boolean =
      [T] => (t: T) => t.hashCode() > 0
    val theGriff: [T] => T => Boolean =
      [T] => (t: T) => whiffle[T](t)
//    val theWhiff: [T] => T => Boolean = whiffle
    def piffle[T](s: Int, t: T): Boolean = true
//    val thePiff: [T] => (Int, T) => Boolean = piffle

    assertEquals(
      Captured(7).piff[Boolean](
        [T] => (s: Int, t: T) => piffle[T](s, t)
      ),
      expected = true
    )
  }

case class Captured[S](s: S):
  def piff[RESULT](
    piffle: [T] => (S, T) => RESULT
  ): RESULT =
    piffle[S](s, s)

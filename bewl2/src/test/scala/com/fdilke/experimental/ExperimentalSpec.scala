package com.fdilke.experimental

import munit.Clue.generate
import munit.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
// import scala.runtime.stdLibPatches.language.experimental.fewerBraces

class ExperimentalSpec extends FunSuite:
  test("kicking the tyres") {
    val obtained = 42
    val expected = 42
    val x: Future[Int] = Future {
      2
    }
//    val y: Future[Int] = Future:
//      2
    val z: Seq[Int] = rattenberg {
      x => x + 1
    }
// one day... get -Yindent-colons to work
//    val y: Future[Int] = Future:
//      2
    assertEquals(obtained, expected)
    assertEquals(z, Seq(2,3,4))
  }

  def rattenberg(f: Int => Int): Seq[Int] =
    Seq(1,2,3) map f

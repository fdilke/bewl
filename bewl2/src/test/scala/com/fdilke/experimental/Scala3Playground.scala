package com.fdilke.experimental

import munit.FunSuite

import scala.concurrent.Future

class Scala3Playground extends FunSuite:
  test("type lambdas") {
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

  test("swapping ? for > ... ?") {
    val x: List[_] = List[Int](1,2,3)
  }

case class Captured[S](s: S):
  def piff[RESULT](
    piffle: [T] => (S, T) => RESULT
  ): RESULT =
    piffle[S](s, s)

//    def runTest[S, M, T](
//      r: S ~> M,
//      s: M ~> T,
//      t: M ~> T
//    )(using
//      x: DOT[S], y: DOT[M], z: DOT[T]
//    ): Unit =
//      s.?=[Unit](t) {
//        [A] => (equalizer: Equalizer[A, M]) => (xx: DOT[A]) ?=> {
//          val e = equalizer.inclusion
//          assert {
//            (s o e) =!= (t o e)
//          }
//          assert {
//            (e o equalizer.restrict(r)) =!= r
//          }
//        }
//      }
//
//    def crapply[RESULT](
//     capture: [S, M, T] => (
//       r: S ~> M,
//       s: M ~> T,
//       t: M ~> T
//     ) => (
//       DOT[S], DOT[M], DOT[T]
//     ) ?=>
//       RESULT
//    ): RESULT =
//      ???
//
//    def happly[RESULT](
//     capture: [S, M, T] => (
//       r: S ~> M,
//       s: M ~> T,
//       t: M ~> T
//     ) => (
//       DOT[S]
//     ) ?=>
//       RESULT
//    ): RESULT =
//      ???
//
////    equalizerSituation[Unit](
//    if (false)
//      happly[Unit](
//        [S, M, T] => (
//          r: S ~> M,
//          s: M ~> T,
//          t: M ~> T
//        ) => (
//          ss: DOT[S]
//        ) ?=>
//          ???
//      )
//
//    if (false)
//      crapply[Unit](
//        [S, M, T] => (
//          r: S ~> M,
//          s: M ~> T,
//          t: M ~> T
//        ) => (
//          ss: DOT[S], mm: DOT[M], tt: DOT[T]
//        ) ?=>
//          runTest[S, M, T](r, s, t)
//      )
//
//    equalizerSituation[Unit](
//      [S, M, T] => (
//        r: S ~> M,
//        s: M ~> T,
//        t: M ~> T
//      ) => (
//        ss: DOT[S], mm: DOT[M], tt: DOT[T]
//      ) ?=>
//        runTest[S, M, T](r, s, t)
//    )

// need to understand context parameters/functions better:
// these should sort of compile, but don't: you have to spell out [T] => xxx on the RHS
//  def hax[T](x: T): T = ???
//  val h: [T] =>> (x: T) => T = hax
//  def pax[T](x: T, y: T): T = ???
//  val p: [T] => (x: T, y: T) => T = pax

//  def max[T](x: T, y: T)(using ord: DOT[T]): T = ???
//  val p: [T] => (x: T, y: T) => (ord: DOT[T]) ?=> T = max

trait Actioner {
  def apply[A]: Set[A]
}

def actioner[B]: Actioner =
  new Actioner {
    def apply[A]: Set[A] = Set.empty
  }

def woosh: Unit =
  actioner[Int].apply[Boolean]
  actioner[Int][Boolean]

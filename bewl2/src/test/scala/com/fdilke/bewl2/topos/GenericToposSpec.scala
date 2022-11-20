package com.fdilke.bewl2.topos

import com.fdilke.bewl2.Topos
import munit.FunSuite

import java.io.File

abstract class GenericToposSpec[
  DOT[_],
  CTXT[_],
  VOID,
  UNIT,
  BEWL,
  >[_, _]
](implicit
 val topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >]
) extends FunSuite:

  import topos.*

  type FOO
  type BAR
  type BAZ
//  implicit val dotFoo: Dot[FOO]
//  implicit val dotBar: Dot[BAR]
//  implicit val dotBaz: Dot[BAZ]

  case class EqualizerSituation[S: Dot, M: Dot, T: Dot](
    r: S ~> M,
    s: M ~> T,
    t: M ~> T
  ):
    def sanityTest: Unit =
      topos.sanityTest(r)
      topos.sanityTest(s)
      topos.sanityTest(t)
      if (s =!= t) {
        throw new IllegalArgumentException("equalizing two arrows that are already equal!")
      }
      assert(
        (s o r) =!= (t o r)
      )

    def apply[RESULT](
      capture: [S, M, T] => (
        r: S ~> M,
        s: M ~> T,
        t: M ~> T
      ) => (
        Dot[S], Dot[M], Dot[T]
      ) ?=>
        RESULT
    ): RESULT =
      capture(r, s, t)

  trait ToposFixtures:
    val foo2bar: FOO ~> BAR
    val foo2baz: FOO ~> BAZ
    val foobar2baz: (FOO, BAR) ~> BAZ
    val monicBar2baz: BAR ~> BAZ
    val foo2ImageOfBar: FOO ~> BAZ
    val equalizerSituation: EqualizerSituation[_, _, _]

  def withTestDots(
     block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit

  withTestDots(runTests)

  private def runTests(
    fixtures: ToposFixtures
  )( using
    Dot[FOO],
    Dot[BAR],
    Dot[BAZ]
  ): Unit = {
    import fixtures.*

    test("identity arrows have sane equality semantics") {
      assert(
        identity[CTXT[FOO]] =!= id[FOO]
      )
      assert(
        id[FOO] =!= id[FOO]
      )
    }

    test("identity arrows obey composition laws") {
      assert( (foo2bar o id[FOO]) =!= foo2bar)
      assert( (id[BAR] o foo2bar) =!= foo2bar)
    }

    test("sane fixtures") {
      sanityTest[FOO]
      sanityTest[BAR]
      sanityTest[BAZ]
      sanityTest(foo2bar)
      sanityTest(foobar2baz)
    }

    test("biproduct diagrams work") {
      sanityTest[(BAR, BAZ)]
      val productArrow: FOO ~> (BAR, BAZ) = foo2bar x foo2baz
      productArrow.sanityTest

      π0[BAR, BAZ].sanityTest
      π1[BAR, BAZ].sanityTest

      assert(
        (π0[BAR, BAZ] o productArrow)  =!= foo2bar
      )
      assert(
        (π1[BAR, BAZ] o productArrow) =!= foo2baz
      )
      val recombine: (BAR, BAZ) ~> (BAR, BAZ) =
        π0[BAR, BAZ] x π1[BAR, BAZ]
      assert(
        recombine =!= id[(BAR, BAZ)]
      )
    }

    test("distinguishes projection arrows") {
      assert(!(
        π0[FOO, FOO] =!= π1[FOO, FOO]
      ))
    }

    test("caches products") {
      val fooXbar1: Dot[(FOO, BAR)] = summon[Dot[(FOO, BAR)]]
      val fooXbar2: Dot[(FOO, BAR)] = summon[Dot[(FOO, BAR)]]
      assert(
        (fooXbar1.asInstanceOf[Object]) eq (fooXbar2.asInstanceOf[Object])
      )
    }

    test("the unit object behaves") {
      sanityTest[UNIT]
      val fooTo1: FOO ~> UNIT = toUnit[FOO]
      fooTo1.sanityTest

      assert(
        (toUnit[BAR] o foo2bar) =!= fooTo1
      )
    }

    test("the zero object behaves") {
      sanityTest[VOID]
      val barFrom0: VOID ~> BAR = fromZero[BAR]
      barFrom0.sanityTest

      assert(
        (foo2bar o fromZero[FOO]) =!= barFrom0
      )
    }

    test("consistently calculates arrows from the initial to the terminal") {
      assert( toUnit[VOID] =!= fromZero[UNIT] )
    }

    test("can construct exponential diagrams") {
      sanityTest[BAR > BAZ]
      val eval: (BAR > BAZ, BAR) ~> BAZ  =
        evaluation[BAR, BAZ]
      sanityTest(eval)
      val foo2bar2baz: FOO ~> (BAR > BAZ) =
        transpose(foobar2baz)
      sanityTest(foo2bar2baz)

      // reconstruct the arrow from its transpose:
      // f(foo, bar) === eval(f*(foo), bar)
      assert( foobar2baz =!=
        arrow[(FOO, BAR), BAZ] { (cFooBar: CTXT[(FOO, BAR)]) =>
          val cFoo: CTXT[FOO] = π0[FOO, BAR](cFooBar)
          val cBar: CTXT[BAR] = π1[FOO, BAR](cFooBar)
          val fnBarBaz: CTXT[BAR > BAZ] = foo2bar2baz(cFoo)
          val cTuple = productMagic[BAR > BAZ, BAR](fnBarBaz, cBar)
          eval(cTuple)
        }
      )
    }

    test("caches exponentials") {
      val fooToBar1: Dot[FOO > BAR] = summon[Dot[FOO > BAR]]
      val fooToBar2: Dot[FOO > BAR] = summon[Dot[FOO > BAR]]
      assert(
        (fooToBar1.asInstanceOf[Object]) eq (fooToBar2.asInstanceOf[Object])
      )
    }

    test("calculates equalizers") {
      equalizerSituation[Unit](
        [S, M, T] => (
          r: S ~> M,
          s: M ~> T,
          t: M ~> T
        ) => (
          ss: Dot[S], mm: Dot[M], tt: Dot[T]
        ) ?=>
          s.?=(t) {
            [A] => (equalizer: Equalizer[A, M]) => (aa: Dot[A]) ?=>
              val e = equalizer.inclusion
              assert {
                (s o e) =!= (t o e)
              }
              assert {
                (e o equalizer.restrict(r)) =!= r
              }
          }
      )
    }

    test("has a subobject classifier") {
      sanityTest[BEWL]
      sanityTest(truth)
      // sanityTest(falsity)
      val char: BAZ ~> BEWL = monicBar2baz.chi
      sanityTest(char)
      assert {
        (char o monicBar2baz) =!= toTrue[BAR]
      }
      val restriction = foo2ImageOfBar \ monicBar2baz
      restriction.sanityTest
      assert {
        (monicBar2baz o restriction) =!= foo2ImageOfBar
      }
    }
  }

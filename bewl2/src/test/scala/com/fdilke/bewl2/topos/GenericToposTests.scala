package com.fdilke.bewl2.topos

import com.fdilke.bewl2.Topos
import munit.FunSuite

import java.io.File

abstract class GenericToposTests[
  DOT[_],
  CTXT[_],
  VOID,
  UNIT,
  →[_, _]
](implicit
 val topos: Topos[DOT, CTXT, VOID, UNIT, →]
) extends FunSuite:

  import topos.*

  type FOO
  implicit val dotFoo: DOT[FOO]
  type BAR
  implicit val dotBar: DOT[BAR]
  type BAZ
  implicit val dotBaz: DOT[BAZ]
  val foo2bar: FOO ~> BAR
  val foo2baz: FOO ~> BAZ
  val foobar2baz: (FOO, BAR) ~> BAZ

//  import ToposHelpers._

  test("identity arrows have sane equality semantics") {
    assert(
      arrow[FOO, FOO] { x => x } =!= id[FOO]
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
    val fooXbar1: DOT[(FOO, BAR)] = dot[(FOO, BAR)]
    val fooXbar2: DOT[(FOO, BAR)] = dot[(FOO, BAR)]
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
    sanityTest[BAR → BAZ]
    val eval: (BAR → BAZ, BAR) ~> BAZ  =
      evaluation[BAR, BAZ]
    sanityTest(eval)
    val foo2bar2baz: FOO ~> (BAR → BAZ) =
      transpose(foobar2baz)
    sanityTest(foo2bar2baz)

    // reconstruct the arrow from its transpose:
    // f(foo, bar) === eval(f*(foo), bar)
    assert( foobar2baz =!=
      arrow[(FOO, BAR), BAZ] { (cFooBar: CTXT[(FOO, BAR)]) =>
        val cFoo: CTXT[FOO] = π0[FOO, BAR](cFooBar)
        val cBar: CTXT[BAR] = π1[FOO, BAR](cFooBar)
        val fnBarBaz: CTXT[BAR → BAZ] = foo2bar2baz(cFoo)
        val cTuple = productMagic[BAR → BAZ, BAR](fnBarBaz, cBar)
        eval(cTuple)
      }
    )
  }



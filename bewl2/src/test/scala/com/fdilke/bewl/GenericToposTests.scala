package com.fdilke.bewl

import com.fdilke.bewl2.Topos
import munit.FunSuite

abstract class GenericToposTests[
  DOT[_],
  CTXT[_],
  UNIT
](implicit
 val topos: Topos[DOT, CTXT, UNIT]
) extends FunSuite:

  import topos._

  type FOO
  implicit val dotFoo: DOT[FOO]
  type BAR
  implicit val dotBar: DOT[BAR]
  type BAZ
  implicit val dotBaz: DOT[BAZ]
  val foo2bar: FOO ~> BAR
  val foo2baz: FOO ~> BAZ

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
    sanityTest(foo2bar)
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

  test("the unit object behaves") {
    sanityTest[UNIT]
    val fooTo1: FOO ~> UNIT = toUnit[FOO]
    fooTo1.sanityTest

    assert(
      (toUnit[BAR] o foo2bar) =!= fooTo1
    )
  }
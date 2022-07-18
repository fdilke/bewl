package com.fdilke.bewl

import com.fdilke.bewl2.Topos
import munit.FunSuite

abstract class GenericToposTests[
  SET[_],
  CTXT[_]
](implicit
 val topos: Topos[SET, CTXT]
) extends FunSuite:

  import topos._

  type FOO
  implicit val dotFoo: SET[FOO]
  type BAR
  implicit val dotBar: SET[BAR]
  type BAZ
  implicit val dotBaz: SET[BAZ]
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

    val pig: FOO ~> BAR = π0[BAR, BAZ] o productArrow
    val hog: FOO ~> BAZ = π1[BAR, BAZ] o productArrow

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

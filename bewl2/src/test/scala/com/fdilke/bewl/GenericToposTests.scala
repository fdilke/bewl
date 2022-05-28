package com.fdilke.bewl

import com.fdilke.bewl2.Topos
import munit.FunSuite


//class Abstract:
//  type T
//  def transform(t: T): T
//  val initial: T

abstract class GenericToposTests[
  SET[_]: Topos
] extends FunSuite:

  val theTopos: Topos[SET] = Topos[SET]
  import theTopos._

  type FOO
  implicit val dotFoo: SET[FOO]
  type BAR
  implicit val dotBar: SET[BAR]
  val foo2bar: FOO ~> BAR


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


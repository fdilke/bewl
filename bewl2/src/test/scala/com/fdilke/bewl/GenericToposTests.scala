package com.fdilke.bewl

import com.fdilke.bewl2.{Monad, Topos}
import munit.FunSuite


//class Abstract:
//  type T
//  def transform(t: T): T
//  val initial: T

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
  val foo2bar: FOO ~> BAR


//  import ToposHelpers._

  test("identity arrows have sane equality semantics") {
    assert(
      arrow[FOO, FOO] { x => monad.eta(x) } =!= id[FOO]
    )
    assert(
      id[FOO] =!= id[FOO]
    )
  }

  test("identity arrows obey composition laws") {
    assert( (foo2bar o id[FOO]) =!= foo2bar)
    assert( (id[BAR] o foo2bar) =!= foo2bar)
  }


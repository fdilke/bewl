package com.fdilke.bewl

import com.fdilke.bewl2.{Monad, Topos}
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

  test("sane fixtures") {
    sanityTest[FOO]
    sanityTest(foo2bar)
  }

  test("biproduct diagrams work") {
//    bar.x(baz).sanityTest
//    (bar.x(baz)) should have(
//      left(bar),
//      right(baz)
//    )
//    val productArrow = foo2bar.x(foo2baz)
//
//    productArrow.sanityTest
//    productArrow should have(
//      source(foo),
//      target(bar.x(baz)),
//      sanityTest(null)
//    )
//
//    bar.x(baz).π0.sanityTest
//    bar.x(baz).π1.sanityTest
//
//    foo(bar) { x =>
//      productArrow(x)._1
//    } shouldBe foo2bar
//
//    foo(baz) { x =>
//      productArrow(x)._2
//    } shouldBe foo2baz
//
//    val fooXbar: BIPRODUCT[FOO, BAR] =
//      foo.x(bar)
//    fooXbar(fooXbar) {
//      ⊕.tupled(fooXbar.pair)
//    } shouldBe fooXbar.identity
  }
// TODO: fix
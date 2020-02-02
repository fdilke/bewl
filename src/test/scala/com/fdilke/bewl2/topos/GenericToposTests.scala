package com.fdilke.bewl2.topos

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

abstract class GenericToposTests[
  DOT[_]: Topos,
  FOO : DOT,
  BAR : DOT,
  BAZ : DOT
] extends AnyFunSpec {

  val foo2bar : FOO => BAR
  val foo2ImageOfBar : FOO => BAZ
  //  TODO: sort this out
  //  val foobar2baz : BiArrow[
  //    FOO,
  //    BAR,
  //    BAZ
  //  ]
  val monicBar2baz: BAR => BAZ

  private final lazy val foo2baz = foo2ImageOfBar // a convenient alias

  // TODO: equalizer situation, with sanity tests

  private val topos: Topos[DOT] = implicitly[Topos[DOT]]
  import topos._

  private val foo: DOT[FOO] = implicitly[DOT[FOO]]
  private val bar: DOT[BAR] = implicitly[DOT[BAR]]
  private val baz: DOT[BAZ] = implicitly[DOT[BAZ]]

  describe(s"The fixtures for ${topos.getClass.getSimpleName}") {
    it("include distinct sane objects") {
      val objects: Set[DOT[_]] = Set(
        foo, bar, baz
      )

      objects should have size 3
      objects foreach { dot: DOT[_] =>
        sanityTest(dot)
      }
    }

    it("include sane arrows whose sources and targets match their names") {
      sanityTest(foo2bar)
      source(foo2bar) shouldBe foo
      target(foo2bar) shouldBe bar

      sanityTest(foo2baz)
      source(foo2baz) shouldBe foo
      target(foo2baz) shouldBe baz

//      foobar2baz.arrow.sanityTest
//      foobar2baz.product shouldBe (foo x bar)
//      foobar2baz.product.left shouldBe foo
//      foobar2baz.product.right shouldBe bar
//      foobar2baz.arrow.source shouldBe (foo x bar)
//      foobar2baz.arrow.target shouldBe baz

      sanityTest(monicBar2baz)
      source(monicBar2baz) shouldBe bar
      target(monicBar2baz) shouldBe baz

//      equalizerSituation.sanityTest
    }
  }

}

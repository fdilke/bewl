package com.fdilke.bewl.topos

import org.scalatest.Matchers._
import org.scalatest._

abstract class ToposFixtureSanityTests[T <: Topos](fixtures: ToposWithFixtures) extends FunSpec {
  import fixtures._

  describe(s"The fixtures for ${fixtures.topos.getClass.getSimpleName}") {
    it("include distinct sane objects") {
      val objects = Set(foo, bar, baz)

      objects should have size 3

      objects map { _.sanityTest }
    }

    it("include sane arrows whose sources and targets match their names") {
      foo2bar.source shouldBe foo
      foo2bar.target shouldBe bar
      foo2bar.sanityTest

      foo2baz.source shouldBe foo
      foo2baz.target shouldBe baz
      foo2baz.sanityTest

//      foobar2baz.arrow.source shouldBe (foo x bar)
//      foobar2baz.arrow.target shouldBe baz
//      foobar2baz.arrow.sanityTest

      monicBar2baz.source shouldBe bar
      monicBar2baz.target shouldBe baz
      monicBar2baz.sanityTest

      equalizerSituation.sanityTest
    }
  }
}

abstract class ToposWithFixtures {
  type TOPOS <: Topos
  val topos : TOPOS

  import topos.ELEMENT

  type FOO <: ELEMENT
  type BAR <: ELEMENT
  type BAZ <: ELEMENT
  type SUB_BAR <: ELEMENT

  import topos._

  def makeSampleStar(): STAR[_ <: ELEMENT]

  def makeSampleQuiver(): QUIVER[_ <: ELEMENT, _ <: ELEMENT]

  val foo : STAR[FOO]
  val bar : STAR[BAR]
  val baz : STAR[BAZ]

  val foo2bar : QUIVER[FOO, BAR]
  val foo2ImageOfBar : QUIVER[FOO, BAZ]
//  val foobar2baz : BiArrow[FOO, BAR, BAZ]
  val monicBar2baz: QUIVER[BAR, BAZ]

  val equalizerSituation: EqualizerSituation[FOO, BAR, BAZ]

  case class EqualizerSituation[S <: ELEMENT, M <: ELEMENT, T <: ELEMENT](
    r: QUIVER[S, M],
    s: QUIVER[M, T],
    t: QUIVER[M, T]) {

    def sanityTest {
      r.sanityTest
      s.sanityTest
      t.sanityTest
    }

    (s o r) shouldBe (t o r)
  }

  final lazy val foo2baz = foo2ImageOfBar // a convenient alias
}

abstract class GenericToposTests[TOPOS <: Topos](
                                fixtures: ToposWithFixtures
) extends ToposFixtureSanityTests(fixtures) {

  import fixtures._
  import fixtures.topos._

  describe(s"The topos ${topos.getClass.getSimpleName}") {

    it("wraps dots and arrows with relatively sane equality semantics") {
      makeSampleStar() shouldBe makeSampleStar()
      (makeSampleStar() eq makeSampleStar()) shouldBe true

      makeSampleQuiver() shouldBe makeSampleQuiver()
      (makeSampleQuiver() eq makeSampleQuiver()) shouldBe false
    }

    it("has identity arrows which can be composed") {
      foo2bar o foo.identity shouldBe foo2bar
      bar.identity o foo2bar shouldBe foo2bar
    }

    it("can construct biproduct diagrams") {
      val barXbaz = bar x baz

      val productArrow = foo2bar x foo2baz

      productArrow.source shouldBe foo
      productArrow.target shouldBe barXbaz

      productArrow.sanityTest

      leftProjection(bar, baz).sanityTest
      rightProjection(bar, baz).sanityTest

      leftProjection(bar, baz) o productArrow shouldBe foo2bar
      rightProjection(bar, baz) o productArrow shouldBe foo2baz
    }

    it("has a terminator") {
      val fooToI = foo.toI
      fooToI.source shouldBe foo
      fooToI.target shouldBe topos.I
      fooToI.sanityTest

      bar.toI o foo2bar shouldBe fooToI
    }

    it("has standardized products") {
      val product: STAR[FOO x BAR] = foo x bar
      product shouldBe (foo x bar)
    }

    it("can chain products") {
      val barXfooXbaz = bar x foo x baz
      val productArrow = foo2bar x foo.identity x foo2baz
      productArrow.source shouldBe foo
      productArrow.target shouldBe barXfooXbaz
      productArrow.sanityTest

      leftProjection(bar, foo, baz) o productArrow shouldBe foo2bar
      midProjection(bar, foo, baz) o productArrow shouldBe foo.identity
      rightProjection(bar, foo, baz) o productArrow shouldBe foo2baz
    }

    it("has equalizers") {
      import equalizerSituation._
      val equalizer = s ?= t
      val e = equalizer.inclusion

      (s o e) shouldBe (t o e)
      val q = equalizer.restrict(r)
      (e o q) shouldBe r
    }


    // TODO: get equivalents of all the other generic tests working
  }
}

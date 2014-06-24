package com.fdilke.bewl

import org.scalatest.Matchers._
import org.scalatest._

abstract class ToposWithFixtures {
  type TOPOS <: Topos
  val topos : TOPOS

  type FOO
  type BAR
  type BAZ
  type SUB_BAR

  import topos._

  val foo : DOT[FOO]
  val bar : DOT[BAR]
  val baz : DOT[BAZ]

  val foo2bar : ARROW[FOO, BAR]
  val foo2ImageOfBar : ARROW[FOO, BAZ]
  val foobar2baz : BiArrow[FOO, BAR, BAZ]
  val monicBar2baz: ARROW[BAR, BAZ]

  val equalizerSituation: EqualizerSituation[FOO, BAR, BAZ]

  case class EqualizerSituation[S, M, T](r: ARROW[S, M], s: ARROW[M, T], t: ARROW[M, T]) {
    def sanityTest {
      r.sanityTest
      s.sanityTest
      t.sanityTest
    }

    s(r) shouldBe t(r)
  }

  final lazy val foo2baz = foo2ImageOfBar // a convenient alias
}

abstract class ToposFixtureSanityTests[T <: Topos](fixtures: ToposWithFixtures) extends FunSpec {
  import fixtures._

  describe(s"The fixtures for ${fixtures.topos.getClass.getSimpleName}") {
    it("include distinct objects") {
      Set(foo, bar, baz) should have size 3
    }

    it("include sane arrows whose sources and targets match their names") {
      foo2bar.source shouldBe foo
      foo2bar.target shouldBe bar
      foo2bar.sanityTest

      foo2baz.source shouldBe foo
      foo2baz.target shouldBe baz
      foo2baz.sanityTest

      foobar2baz.arrow.source shouldBe (foo x bar)
      foobar2baz.arrow.target shouldBe baz
      foobar2baz.arrow.sanityTest

      monicBar2baz.source shouldBe bar
      monicBar2baz.target shouldBe baz
      monicBar2baz.sanityTest

      equalizerSituation.sanityTest
    }
  }
}

abstract class GenericToposTests[TOPOS <: Topos](
    fixtures: ToposWithFixtures
  ) extends ToposFixtureSanityTests(fixtures) {

  import fixtures._
  import fixtures.topos._

  describe(s"The topos ${topos.getClass.getSimpleName}") {
    it("has identity arrows which can be composed") {
      foo2bar(foo.identity) shouldBe foo2bar
      bar.identity(foo2bar) shouldBe foo2bar
    }

    it("can construct biproduct diagrams") {
      val barXbaz = bar x baz

      val productArrow = foo2bar x foo2baz
      productArrow.source shouldBe foo
      productArrow.target shouldBe barXbaz
      productArrow.sanityTest

      leftProjection(bar, baz).sanityTest
      rightProjection(bar, baz).sanityTest

      leftProjection(bar, baz)(productArrow) shouldBe foo2bar
      rightProjection(bar, baz)(productArrow) shouldBe foo2baz
    }

    it("has a terminator") {
      val fooToI = foo.toI
      fooToI.source shouldBe foo
      fooToI.target shouldBe topos.I
      fooToI.sanityTest

      bar.toI(foo2bar) shouldBe fooToI
    }

    it("has standardized products") {
      val product: DOT[(FOO, BAR)] = foo x bar
      product shouldBe (foo x bar)
    }

    it("can chain products") {
      val barXfooXbaz = bar x foo x baz
      val productArrow = foo2bar x foo.identity x foo2baz
      productArrow.source shouldBe foo
      productArrow.target shouldBe barXfooXbaz
      productArrow.sanityTest

      leftProjection(bar, foo, baz)(productArrow) shouldBe foo2bar
      midProjection(bar, foo, baz)(productArrow) shouldBe foo.identity
      rightProjection(bar, foo, baz)(productArrow) shouldBe foo2baz
    }

    it("can construct exponential diagrams") {
      // Check evaluation maps baz^bar x bar -> baz
      val ev = evaluation(bar, baz)
      ev.left shouldBe baz ^ bar
      ev.right shouldBe bar
      ev.arrow.target shouldBe baz
      ev.arrow.sanityTest

      val tran: ARROW[FOO, BAR => BAZ] = transpose(bar, baz, foobar2baz)
      tran.sanityTest
      tran should have('source(foo), 'target(ev.left))

      // Next, construct the arrow: transpose x 1 : foo x baz -> bar^baz x baz
      // as the product of foo x baz -> foo -> bar^baz and foo x baz -> baz -> baz
      foobar2baz.arrow shouldBe ev.arrow(
        tran(leftProjection(foo, bar)) x rightProjection(foo, bar))
    }

    it("has standardized exponentials") {
      val exponential: DOT[BAR => FOO] = foo ^ bar
      exponential shouldBe (foo ^ bar)
    }

    it("has equalizers") {
      import equalizerSituation._
      val diagram: EQUALIZER[BAR, BAZ] = s ?= t
      val e: ARROW[EQUALIZER_SOURCE[BAR, BAZ], BAR] = diagram.equalizer

      s(e) shouldBe t(e)
      val q: ARROW[FOO, EQUALIZER_SOURCE[BAR, BAZ]] = diagram.restrict(r)
      e(q) shouldBe r
    }

    it("has a subobject classifier") {
      truth.source shouldBe I
      truth.target shouldBe omega

      // given: monic bar -> baz
      // and: foo -> baz whose image lies in the image of bar

      val char = monicBar2baz.chi
      char.arrow.source shouldBe baz
      char.arrow.target shouldBe omega
      char.arrow(monicBar2baz) shouldBe truth(bar.toI)

      val restriction = char.restrict(foo2ImageOfBar)
      restriction.source shouldBe foo
      restriction.target shouldBe bar
      monicBar2baz(restriction) shouldBe foo2ImageOfBar
    }
  }
}



package com.fdilke.bewl.topos

import org.scalatest.Matchers._
import org.scalatest._

abstract class ToposFixtureSanityTests[T <: BaseTopos](fixtures: ToposWithFixtures) extends FunSpec {
  import fixtures._

  describe(s"The fixtures for ${fixtures.topos.getClass.getName}") {
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

      foobar2baz.product.left shouldBe foo
      foobar2baz.product.right shouldBe bar
      foobar2baz.quiver.source shouldBe (foo x bar)
      foobar2baz.quiver.target shouldBe baz
      foobar2baz.quiver.sanityTest

      monicBar2baz.source shouldBe bar
      monicBar2baz.target shouldBe baz
      monicBar2baz.sanityTest

      equalizerSituation.sanityTest
    }
  }
}

abstract class ToposWithFixtures {
  val topos : Topos

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
  val foobar2baz : BiQuiver[FOO, BAR, BAZ]
  val monicBar2baz: QUIVER[BAR, BAZ]

  val equalizerSituation: EqualizerSituation[_ <: ELEMENT, _ <: ELEMENT, _ <: ELEMENT]

  case class EqualizerSituation[S <: ELEMENT, M <: ELEMENT, T <: ELEMENT](
    r: QUIVER[S, M],
    s: QUIVER[M, T],
    t: QUIVER[M, T]) {

    def sanityTest {
      r.sanityTest
      s.sanityTest
      t.sanityTest
    }

    if (s == t) {
      throw new IllegalArgumentException("equalizing two quivers that are already equal!")
    }

    (s o r) shouldBe (t o r)
  }

  final lazy val foo2baz = foo2ImageOfBar // a convenient alias
}

abstract class GenericToposTests[TOPOS <: BaseTopos](
                                fixtures: ToposWithFixtures
) extends ToposFixtureSanityTests(fixtures) {

  import fixtures._
  import fixtures.topos._

  describe(s"The topos ${topos.getClass.getName}") {

    it("has sane built-in objects") {
      I.sanityTest
      omega.sanityTest
    }

    it("wraps dots and arrows with relatively sane equality semantics") {
      makeSampleStar() shouldBe makeSampleStar()
      (makeSampleStar() eq makeSampleStar()) shouldBe true

      makeSampleQuiver() shouldBe makeSampleQuiver()
      (makeSampleQuiver() eq makeSampleQuiver()) shouldBe false
    }

    it("has identity arrows which can be composed") {
      foo(foo)(identity) shouldBe foo.identity
      foo2bar o foo.identity shouldBe foo2bar
      bar.identity o foo2bar shouldBe foo2bar
    }

    it("can construct biproduct diagrams") {
      (bar x baz) should have(
        'left (bar),
        'right (baz)
      )
      val productQuiver = foo2bar x foo2baz

      productQuiver should have (
        'source (foo),
        'target (bar x baz),
        'sanityTest (null)
      )

      (bar x baz).π0.sanityTest
      (bar x baz).π1.sanityTest

      foo(bar) {
        x => productQuiver(x)._1
      } shouldBe foo2bar

      foo(baz) {
        x => productQuiver(x)._2
      } shouldBe foo2baz

      val fooXbar = foo x bar
      fooXbar(fooXbar) {
        case (f, b) => fooXbar.pair(f, b)
      } shouldBe fooXbar.identity
    }

    it("has a terminator") {
      val fooToI = foo.toI
      fooToI.source shouldBe foo
      fooToI.target shouldBe topos.I
      fooToI.sanityTest

      bar.toI o foo2bar shouldBe fooToI
    }

    it("has standardized products") {
      (foo x bar) shouldBe (foo x bar)
    }

    it("can chain products") {
      val barXfooXbaz = bar x foo x baz
      val productQuiver = foo2bar x foo.identity x foo2baz
      productQuiver.source shouldBe foo
      productQuiver.target shouldBe barXfooXbaz
      productQuiver.sanityTest

      leftProjection(bar, foo, baz) o productQuiver shouldBe foo2bar
      midProjection(bar, foo, baz) o productQuiver shouldBe foo.identity
      rightProjection(bar, foo, baz) o productQuiver shouldBe foo2baz
    }

    it("can construct exponential diagrams") {
      // Check evaluation maps baz^bar x bar -> baz
      val evaluation = (bar > baz).evaluation
      evaluation.product.left shouldBe (bar > baz)
      evaluation.product.right shouldBe bar
      evaluation.quiver.target shouldBe baz
      evaluation.quiver.sanityTest

      val foo2bar2baz: QUIVER[FOO, BAR > BAZ] = (bar > baz).transpose(foobar2baz)
      foo2bar2baz.sanityTest
      foo2bar2baz should have(
        'source(foo),
        'target(bar > baz)
      )

      (foo x bar)(baz) {
        case (f, b) =>
          foo2bar2baz(f)(b)
      } shouldBe foobar2baz.quiver
    }

    it("has standardized exponentials") {
      (foo > bar) shouldBe (foo > bar)
    }

    it("has equalizers", Tag("eq")) {
      // minor hackery required to extract the types
      def runTest[S <: ELEMENT, M <: ELEMENT, T <: ELEMENT](situation: EqualizerSituation[S, M, T]) {
          import situation._
          val equalizer = s ?= t
          val e = equalizer.inclusion

          (s o e) shouldBe (t o e)
          (e o equalizer.restrict(r)) shouldBe r
      }
      runTest(equalizerSituation)
    }

    it("has a truth object (subobject classifier)") {
      truth.source shouldBe I
      truth.target shouldBe omega

      val char = monicBar2baz.chi
      char.source shouldBe baz
      char.target shouldBe omega

//      {
//        val com = char o monicBar2baz
//        val b2t = bar.toTrue
//
//        com.source shouldBe b2t.source
//        com.target shouldBe b2t.target
//        println("Glory be")
//      }

      char o monicBar2baz shouldBe bar.toTrue

      val restriction = foo2ImageOfBar \ monicBar2baz
      restriction.source shouldBe foo
      restriction.target shouldBe bar
      monicBar2baz o restriction shouldBe foo2ImageOfBar

      // TODO: contruct a non-monic arrow, have chi throw a NotMonicException
      // TODO: try backdividing by a monic when we can't
    }

/*
    ignore("has a Heyting algebra structure for the truth object") {
      omegaHeyting.isInstanceOf[HeytingAlgebra[OMEGA]] shouldBe true
      //      omegaHeyting.verify
    }
 */
    // TODO: make the commented-out tests work
  }
}

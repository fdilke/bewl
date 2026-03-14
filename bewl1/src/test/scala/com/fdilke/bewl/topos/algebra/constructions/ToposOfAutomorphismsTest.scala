package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsPreArrow}
import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import FiniteSets.{>, ToposOfAutomorphisms}
import ToposOfAutomorphisms.AutomorphismPreArrow
import scala.Function._
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl.helper.StandardSymbols.{a, b, c, d, boolean}

class ToposOfAutomorphismsTest
    extends GenericToposTests[
      Any,
      Any,
      ({ type λ[X] = X > X })#λ,
      ({ type λ[X, Y] = AutomorphismPreArrow[X, Y] })#λ,
      ({ type λ[T] = T })#λ
    ](
      new ToposWithFixtures[
        Any,
        Any,
        ({ type λ[X] = X > X })#λ,
        ({ type λ[X, Y] = AutomorphismPreArrow[X, Y] })#λ,
        ({ type λ[T] = T })#λ
      ] {

        override val topos = FiniteSets.ToposOfAutomorphisms.build

        import topos._

        override type FOO = Symbol
        override type BAR = Int
        override type BAZ = String

        override val foo = dot(a, b, c, d)(a -> c, b -> d, c -> a, d -> b)

        override val bar = dot(1, 2)(1 -> 2, 2 -> 1)

        override val baz = dot("x", "y", "z", "w")(
          "x" -> "y",
          "y" -> "x",
          "z" -> "z",
          "w" -> "w"
        )

        override val foo2bar = foo(bar)(
          Map(
            a -> 1,
            b -> 2,
            c -> 2,
            d -> 1
          )
        )

        override val monicBar2baz = bar(baz)(
          Map(
            1 -> "x",
            2 -> "y"
          )
        )

        override val foo2ImageOfBar = foo(baz)(
          Map(
            a -> "x",
            b -> "x",
            c -> "y",
            d -> "y"
          )
        )

        override val foobar2baz = bifunctionAsBiArrow(foo, bar, baz)(
          untupled(
            Map(
              (a, 1) -> "x",
              (b, 1) -> "y",
              (c, 1) -> "y",
              (d, 1) -> "y",
              (a, 2) -> "x",
              (b, 2) -> "x",
              (c, 2) -> "y",
              (d, 2) -> "x"
            )
          )
        )

        private def dot[A](values: A*)(mappings: (A, A)*) = {
          val set: Set[A] = Set(values: _*)
          makeDot(
            FiniteSets.makeArrow(
              FiniteSetsPreArrow(
                set,
                set,
                Map(mappings: _*)
              )
            )
          )
        }

        override def makeSampleDot() =
          dot(1, 2, 3)(1 -> 2, 2 -> 3, 3 -> 1)

        override def makeSampleArrow() =
          makeSampleDot()(makeSampleDot())(
            Map(
              1 -> 3,
              3 -> 2,
              2 -> 1
            )
          )

        override val equalizerSituation = new EqualizerSituation[UNIT, FOO, BAZ](
          dot[UNIT]()()(foo)(Map()),
          foo2ImageOfBar,
          monicBar2baz o foo2bar
        )
      }
    ) {
  import fixtures._

  describe("The Boolean property") {
    it("holds") { // too slow :( :( :(
      topos shouldBe boolean
    }
  }

  import topos._

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have size 2
    }

    it("works on the fixtures") {
      foo.globals shouldBe empty
      bar.globals shouldBe empty
      baz.globals should have size 2
    }
  }

  describe("Sizing") {
    it("works on the built-ins") {
      omega should have size 2
    }

    it("works on the fixtures") {
      foo should have size 4
      bar should have size 2
      baz should have size 4
    }
  }

  describe("Arrow enumeration") {
    it("also works on the fixtures") {
      (omega >> omega) should have size 4
    }

    it("confirms our intuitions") {
      foo >> foo should have size 16
      foo >> bar should have size 4
      foo >> baz should have size 16

      (foo >> (omega > omega)) should have size 16
      (foo x omega) >> omega should have size 16
    }
  }
}

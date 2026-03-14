package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.helper.StandardSymbols.{a, boolean, i}
import com.fdilke.bewl.topos.algebra.KnownGroups.twoGroup
import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import org.scalatest.matchers.should.Matchers.{a => _, _}

import scala.Function.untupled

class ToposOfGroupActionsTest
    extends GenericToposTests[
      Any,
      Any,
      ({ type λ[X] = twoGroup.Action[X] })#λ,
      ({ type λ[X, Y] = twoGroup.ActionPreArrow[X, Y] })#λ,
      ({ type λ[T] = T })#λ
    ](
      new ToposWithFixtures[
        Any,
        Any,
        ({ type λ[X] = twoGroup.Action[X] })#λ,
        ({ type λ[X, Y] = twoGroup.ActionPreArrow[X, Y] })#λ,
        ({ type λ[T] = T })#λ
      ] {

        override val topos = FiniteSets.ToposOfGroupActions of twoGroup

        import topos.{~ => _, _}

        override type FOO = Symbol
        override type BAR = String
        override type BAZ = Int

        override val foo = makeDot(twoGroup.regularAction)

        private val barDot: FiniteSets.DOT[String] = FiniteSetsUtilities.dot("x", "x'", "y")

        private val barFlip: String => String = Map("x" -> "x'", "x'" -> "x", "y" -> "y")

        private val barMultiply: (String, Symbol) => String =
          (s, m) => if (m == a) barFlip(s) else s

        override val bar = makeDot(twoGroup.action(barDot)(barMultiply))

        private val bazDot: FiniteSets.DOT[Int] = FiniteSetsUtilities.dot(1, 2, 3, 4, 5)

        // TODO: pack away local private stuff into a scope

        private val bazFlip: Int => Int = Map(1 -> 2, 2 -> 1, 3 -> 4, 4 -> 3, 5 -> 5)

        private val bazMultiply: (Int, Symbol) => Int =
          (s, m) => if (m == a) bazFlip(s) else s

        override val baz = makeDot(twoGroup.action(bazDot)(bazMultiply))

        override val foo2bar = functionAsArrow(foo, bar, Map(i -> "x", a -> "x'"))
        override val foo2ImageOfBar = functionAsArrow(foo, baz, Map(i -> 4, a -> 3))
        override val foobar2baz = bifunctionAsBiArrow(foo, bar, baz)(
          untupled(
            Map(
              (i, "x") -> 1,
              (i, "x'") -> 3,
              (i, "y") -> 2,
              (a, "x") -> 4,
              (a, "x'") -> 2,
              (a, "y") -> 1
            )
          )
        )
        override val monicBar2baz = functionAsArrow(bar, baz, Map("x" -> 3, "x'" -> 4, "y" -> 5))

        override def makeSampleDot(): DOT[String] =
          makeDot(twoGroup.action(barDot)(barMultiply))

        override def makeSampleArrow(): Symbol > String =
          functionAsArrow(
            foo,
            bar,
            Map(
              i -> "x",
              a -> "x'"
            )
          )

        override val equalizerSituation = {
          val altMonicBar2baz = functionAsArrow(bar, baz, Map("x" -> 2, "x'" -> 1, "y" -> 5))
          val pickOutY = tempConst(bar)("y")

          new EqualizerSituation[UNIT, String, Int](
            pickOutY,
            monicBar2baz,
            altMonicBar2baz
          )
        }
      }
    ) {
  import fixtures._
  import topos.{~ => _, _}

  describe("The Boolean property") {
    it("holds") {
      topos shouldBe boolean
    }
  }

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have size 2
    }

    it("works on the fixtures") {
      foo.globals shouldBe empty
      bar.globals should have size 1
      baz.globals should have size 1
    }
  }

  describe("Sizing") {
    it("works on the built-ins") {
      omega should have size 2
    }

    it("works on the fixtures") {
      foo should have size 2
      bar should have size 3
      baz should have size 5
    }
  }

  describe("Arrow enumeration") {
    it("also works on the fixtures") {
      (omega >> omega) should have size 4
    }

    it("...optional extras") {
      foo >> foo should have size 2
      foo >> bar should have size 3
      foo >> baz should have size 5

      (foo >> (omega > omega)) should have size 4
    }
  }
}

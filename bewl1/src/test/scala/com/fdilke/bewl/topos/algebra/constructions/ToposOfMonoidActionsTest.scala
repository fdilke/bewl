package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.FiniteSets.{ToposOfMonoidActions, ~}
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import com.fdilke.bewl.topos.{GenericToposTests, Topos, ToposWithFixtures, Wrappings}
import org.scalatest.matchers.should.Matchers.{a => _, _}
import com.fdilke.bewl.helper.StandardSymbols.{i, x, y, minimal, boolean, iso}

import scala.Function.untupled

class UnoptimizedToposOfActionsTest
    extends ToposOfMonoidActionsTest(
      ToposOfMonoidActions of (
        monoidOf3,
        FiniteSets.DefaultMonoidAssistant
      )
    )

class OptimizedToposOfActionsTest
    extends ToposOfMonoidActionsTest(
      ToposOfMonoidActions of monoidOf3
    )

class ToposOfMonoidActionsTest(
    actionTopos: Topos[~]
      with Wrappings[
        ~,
        ~,
        ({ type λ[X <: ~] = monoidOf3.Action[X] })#λ,
        ({ type λ[X <: ~, Y <: ~] = monoidOf3.ActionPreArrow[X, Y] })#λ,
        ({ type λ[T <: ~] = T })#λ
      ]
) extends GenericToposTests[
      ~,
      ~,
      ({ type λ[X <: ~] = monoidOf3.Action[X] })#λ,
      ({ type λ[X <: ~, Y <: ~] = monoidOf3.ActionPreArrow[X, Y] })#λ,
      ({ type λ[T <: ~] = T })#λ
    ](
      new ToposWithFixtures[
        ~,
        ~,
        ({ type λ[X <: ~] = monoidOf3.Action[X] })#λ,
        ({ type λ[X <: ~, Y <: ~] = monoidOf3.ActionPreArrow[X, Y] })#λ,
        ({ type λ[T <: ~] = T })#λ
      ] {

        override val topos = actionTopos
        import topos._

        override type FOO = Symbol
        override type BAR = String
        override type BAZ = String

        override val foo = makeDot(monoidOf3.regularAction)

        private val barDot: FiniteSets.DOT[String] = FiniteSetsUtilities.dot("x", "y")
        private val bazDot: FiniteSets.DOT[String] = FiniteSetsUtilities.dot("i", "x", "y")

        private val scalarMultiply: (String, Symbol) => String =
          (s, m) => monoidOf3.multiply(Symbol(s), m).name

        override val bar = makeDot(monoidOf3.action(barDot)(scalarMultiply))
        override val baz = makeDot(monoidOf3.action(bazDot)(scalarMultiply))

        override val foo2ImageOfBar = functionAsArrow(foo, baz, Map(i -> "y", x -> "x", y -> "y"))
        override val foo2bar = functionAsArrow(foo, bar, Map(i -> "x", x -> "x", y -> "y"))
        override val foobar2baz = bifunctionAsBiArrow(foo, bar, baz)(
          untupled(
            Map(
              (i, "x") -> "x",
              (x, "x") -> "x",
              (y, "x") -> "y",
              (i, "y") -> "y",
              (x, "y") -> "x",
              (y, "y") -> "y"
            )
          )
        )
        override val monicBar2baz = functionAsArrow(bar, baz, Map("x" -> "x", "y" -> "y"))

        override def makeSampleDot(): DOT[String] =
          makeDot(monoidOf3.action(barDot)(scalarMultiply))

        override def makeSampleArrow(): Symbol > String =
          functionAsArrow(
            foo,
            bar,
            Map(
              i -> "x",
              x -> "x",
              y -> "y"
            )
          ) // left multiplication by x

        override val equalizerSituation = {
          val wizDot: FiniteSets.DOT[Int] = FiniteSetsUtilities.dot(0, 1, 2, 3)

          def wizMultiply(n: Int, r: Symbol): Int =
            if (n == 0)
              0
            else
              r match {
                case `i` => n
                case `x` => 1
                case `y` => 2
              }

          val wizAction = monoidOf3.action(wizDot)(wizMultiply)

          val wiz = makeDot(wizAction)

          val foo2wiz =
            functionAsArrow(
              foo,
              wiz,
              Map(
                i -> 1,
                x -> 1,
                y -> 2
              )
            )

          type WIZ = Int
          type BINARY = Boolean
          val binaryDot: FiniteSets.DOT[Boolean] =
            FiniteSetsUtilities.dot(true, false)
          def binaryMultiply(b: Boolean, r: Symbol): Boolean = b
          val binary = makeDot(
            monoidOf3.action(binaryDot)(binaryMultiply)
          )
          new EqualizerSituation[FOO, WIZ, BINARY](
            foo2wiz,
            functionAsArrow(
              wiz,
              binary,
              Map(
                0 -> true,
                1 -> true,
                2 -> true,
                3 -> true
              )
            ),
            functionAsArrow(
              wiz,
              binary,
              Map(
                0 -> false,
                1 -> true,
                2 -> true,
                3 -> true
              )
            )
          )
        }
      }
    ) {
  import fixtures._
  import topos._

  describe("The O object") {
    it("is isomorphic to the empty action") {
      topos
        .makeDot(
          monoidOf3.voidAction
        )
        .fromO shouldBe iso
    }
  }

  describe("The Boolean property") {
    it("fails") {
      topos should not be boolean
    }
  }

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have size 2
    }

    it("works on the fixtures") {
      foo.globals shouldBe empty
      bar.globals shouldBe empty
      baz.globals shouldBe empty
    }
  }

  describe("Sizing") {
    it("works on the built-ins") {
      omega should have size 3
    }

    it("works on the fixtures") {
      foo should have size 3
      bar should have size 2
      baz should have size 3
    }
  }

  describe("Arrow enumeration") {
    it("also works on the fixtures") {
      (omega >> omega) should have size 6
    }

    it("...optional extras") {
      foo >> foo should have size 3
      foo >> bar should have size 2
      foo >> baz should have size 3

      // probably not - that would be if we were only counting isomorphisms
//      (foo >> (omega > omega)) should have size 2
    }
  }

  describe("Minimality") {
    it("should hold for the right objects") {
      foo shouldNot be(minimal)
      bar should be(minimal)
      baz shouldNot be(minimal)
      O shouldNot be(minimal)
      I should be(minimal)
      omega shouldNot be(minimal)
    }
  }
}

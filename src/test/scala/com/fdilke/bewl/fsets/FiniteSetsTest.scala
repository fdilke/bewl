package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.StandardSymbols.boolean
import com.fdilke.bewl.topos.Wrappings.NO_WRAPPER
import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import org.scalatest.matchers.should.Matchers._

class FiniteSetsTest
  extends GenericToposTests[
    Any,
    Any,
    Iterable,
    FiniteSetsPreArrow,
    NO_WRAPPER
  ](
    new ToposWithFixtures[
      Any,
      Any,
      Iterable,
      FiniteSetsPreArrow,
      NO_WRAPPER
    ] {
      val topos = FiniteSets

      type FOO = Boolean
      type BAR = String
      type BAZ = Int

      override val foo = dot(true, false)
      override val bar = dot("X", "Y", "Z")
      override val foo2bar = arrow(foo, bar)(true -> "X", false -> "Y")
      override val baz = dot(1, 2, 3, 4)
      override val foo2ImageOfBar = arrow(foo, baz)(true -> 3, false -> 2)

      override val foobar2baz = biArrow(
        foo,
        bar,
        baz
      )(
        (true, "X") -> 2,
        (false, "X") -> 3,
        (true, "Y") -> 1,
        (false, "Y") -> 2,
        (true, "Z") -> 2,
        (false, "Z") -> 3
      )

      override val monicBar2baz = arrow(
        bar,
        baz
      )(
        "X" -> 2,
        "Y" -> 3,
        "Z" -> 1
      )

      override def makeSampleDot() =
        dot(1, 2)

      override def makeSampleArrow() =
        arrow(
          dot(1, 2),
          dot(true, false)
        )(
          1 -> true,
          2 -> false
        )

      override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
        foo2bar,
        arrow(bar, baz)("X" -> 1, "Y" -> 2, "Z" -> 3),
        arrow(bar, baz)("X" -> 1, "Y" -> 2, "Z" -> 1)
      )
    }
  ) {
  import fixtures._
  import topos._

  describe("The Boolean property") {
    it("holds") {
      topos shouldBe boolean
    }
  }

  describe("Sizing") {
    it("works on the built-ins") {
      omega should have size 2
    }

    it("works on the fixtures") {
      foo should have size 2
      bar should have size 3
      baz should have size 4
    }
  }
}

package com.fdilke.bewl.fsets

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import FiniteSetsUtilities._
import org.scalatest.Matchers._

class FiniteSetsTest extends GenericToposTests(new ToposWithFixtures[Any] {
  val topos = FiniteSets

  type FOO = Boolean
  type BAR = String
  type BAZ = Int

  override val foo = dot(true, false)
  override val bar = dot("X", "Y", "Z")
  override val foo2bar = arrow(foo, bar, true -> "X", false -> "Y")
  override val baz = dot(1, 2, 3, 4)
  override val foo2ImageOfBar = arrow(foo, baz, true -> 3, false -> 2)

  override val foobar2baz = biArrow(
    foo, bar, baz,
    (true, "X") -> 2, (false, "X") -> 3,
    (true, "Y") -> 1, (false, "Y") -> 2,
    (true, "Z") -> 2, (false, "Z") -> 3
  )

  override val monicBar2baz = arrow(
    bar, baz, "X" -> 2, "Y" -> 3, "Z" -> 1
  )

  override def makeSampleDot() = dot(1, 2)

  override def makeSampleArrow() =
    arrow(dot(1, 2), dot(true, false), 1 -> true, 2 -> false)

  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
    foo2bar,
    arrow(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
    arrow(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
  )
}) {
  import fixtures._
  import topos._

  describe("The Boolean property") {
    it("holds") {
      topos should be('boolean)
    }
  }

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have size 2
    }

    it("works on the fixtures") {
      foo.globals should have size 2
      bar.globals should have size 3
      baz.globals should have size 4
    }
  }
}




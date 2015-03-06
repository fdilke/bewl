package com.fdilke.bewl.fsets

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import FiniteSetsUtilities._
import org.scalatest.Matchers._

class FiniteSetsTest extends GenericToposTests(new ToposWithFixtures {
  val topos = FiniteSets

  import topos._

  type FOO = Boolean
  type BAR = String
  type BAZ = Int

  override val foo = makeStar(true, false)
  override val bar = makeStar("X", "Y", "Z")
  override val foo2bar = makeQuiver(foo, bar, true -> "X", false -> "Y")
  override val baz = makeStar(1, 2, 3)
  override val foo2ImageOfBar = makeQuiver(foo, baz, true -> 3, false -> 2)

  override val foobar2baz = makeBiQuiver(
    foo, bar, baz,
    (true, "X") -> 2, (false, "X") -> 3,
    (true, "Y") -> 1, (false, "Y") -> 2,
    (true, "Z") -> 2, (false, "Z") -> 3
  )

  override val monicBar2baz = makeQuiver(
    bar, baz, "X" -> 2, "Y" -> 3, "Z" -> 1
  )

  private val sampleDotSource = DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot(Seq(1, 2))
  private val sampleDotTarget = DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot(Seq(true, false))

  override def makeSampleStar() = star(sampleDotSource)

  override def makeSampleQuiver() = makeQuiver(star(sampleDotSource), star(sampleDotTarget), 1 -> true, 2 -> false)

  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
    foo2bar,
    makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 3),
    makeQuiver(bar, baz, "X" -> 1, "Y" -> 2, "Z" -> 1)
  )
}) {
  import fixtures._
  import fixtures.topos._

  describe("Global element enumeration") {
    it("works on the built-ins") {
      omega.globals should have('size(2))
    }

    it("works on the fixtures") {
      foo.globals should have('size(2))
      bar.globals should have('size(3))
      baz.globals should have('size(3))
    }
  }
}




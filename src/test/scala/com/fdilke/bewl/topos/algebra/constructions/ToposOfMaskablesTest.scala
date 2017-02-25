package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.topos.{GenericToposTests, ToposWithFixtures}
import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.Matchers._

class ToposOfMaskablesTest extends GenericToposTests(new ToposWithFixtures[Any] {
  val topos = FiniteSets.Maskables

  type FOO = Boolean
  type BAR = String
  type BAZ = Int

  def maskedDot[T](
    elements: T*
  ) = 
    topos.makeDot(
      FiniteSets.Mask(
        dot(elements :_*)
      )
    )

  def maskedArrow[S, T](
    source: topos.DOT[S],
    target: topos.DOT[T]
  ) (
    mappings: (S, T)*
  ) =
    source(
      target
    )(
      Map(mappings: _*)
    )  

  def maskedBiArrow[L, R, T](
    left:   topos.DOT[L],
    right:  topos.DOT[R],
    target: topos.DOT[T]
  ) (
    mappings: ((L, R), T)*
  ) = 
    topos.bifunctionAsBiArrow(
      left,
      right,
      target
    ) (
      Function untupled Map(mappings: _*)
    )
    
  override val foo = maskedDot(true, false)
  override val bar = maskedDot("X", "Y", "Z")
  override val foo2bar = maskedArrow(foo, bar)(true -> "X", false -> "Y")
  override val baz = maskedDot(1, 2, 3, 4)
  override val foo2ImageOfBar = maskedArrow(foo, baz)(true -> 3, false -> 2)

  override val foobar2baz = maskedBiArrow(
    foo, bar, baz
  ) (
    (true, "X") -> 2, (false, "X") -> 3,
    (true, "Y") -> 1, (false, "Y") -> 2,
    (true, "Z") -> 2, (false, "Z") -> 3
  )

  override val monicBar2baz = maskedArrow(
    bar,
    baz
  )(
    "X" -> 2,
    "Y" -> 3,
    "Z" -> 1
  )

  override def makeSampleDot() =
    maskedDot(1, 2)

  override def makeSampleArrow() =
    maskedArrow(
      maskedDot(1, 2),
      maskedDot(true, false)
    )(
      1 -> true,
      2 -> false
    )

  override val equalizerSituation = new EqualizerSituation[FOO, BAR, BAZ](
    foo2bar,
    maskedArrow(bar, baz)("X" -> 1, "Y" -> 2, "Z" -> 3),
    maskedArrow(bar, baz)("X" -> 1, "Y" -> 2, "Z" -> 1)
  )
}) {
  import fixtures._
  import topos._

  describe("The Boolean property") {
    it("holds") {
      topos shouldBe 'boolean
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


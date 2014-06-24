package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities.{arrow, dot}
import org.scalatest.FunSpec
import org.scalatest.Matchers._

// TODO; purge ShouldMatchers

class FiniteSetsArrowTest extends FunSpec {
  val aDot = dot("a", "b")
  val dotBig = dot("a", "b", "c")
  val dotSmall = dot("a")
  val doodah = dot(1,2,3)
  val dash = dot("X", "Y", "Z")
  val dashBig = dot("X", "Y", "Z", "W")
  val dot2dash = arrow(aDot, dash, "a"->"X", "b"->"Y")
  val dot2dashBadValues = arrow(aDot, dash, "a"->"boojum", "b"->"heejum")
  val dot2dash_2 = arrow(aDot, dash, "a"->"X", "b"->"Y")
  val dotSmall2dash = arrow(dotSmall, dash, "a"->"X", "b"->"Y")
  val dot2dashBig = arrow(aDot, dashBig, "a"->"X", "b"->"Y")
  val dotBig2dash = arrow(dotBig, dash, "a"->"X", "b"->"Y")
  val doodah2dot = arrow(doodah, aDot, 1->"a", 2->"b", 3->"a")

  describe("An arrow representing a morphism of finite sets") {
    it("should make accessible its source and target") {
      dot2dash.source shouldBe aDot
      dot2dash.target shouldBe dash
    }

    it("should have sensible equality semantics") {
      dot2dash shouldBe dot2dash
      dot2dash shouldBe dot2dash_2
      dot2dash should not be dotSmall2dash
      dot2dash should not be dot2dashBig
    }

    it("should support sanity tests") {
      dot2dash.sanityTest
      dot2dashBig.sanityTest
      doodah2dot.sanityTest
      dotSmall2dash.sanityTest

      intercept[NoSuchElementException] {
        dotBig2dash.sanityTest
      }.getMessage shouldBe "key not found: c"

      intercept[IllegalArgumentException] {
        dot2dashBadValues.sanityTest
      }.getMessage shouldBe "Map values not in target"
    }

    it("should compose with other appropriately conditioned arrows") {
      intercept[IllegalArgumentException] {
        dot2dash(dot2dash)
      }.getMessage shouldBe "Target does not match source"

      dot2dash(doodah2dot) shouldBe
        arrow(doodah, dash, 1->"X", 2->"Y", 3->"X")
    }
  }
}

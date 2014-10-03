package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.FiniteSetsUtilities.{arrow, set}
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class FiniteSetsArrowTest extends FunSpec {
  val dot = set("a", "b")
  val dotBig = set("a", "b", "c")
  val dotSmall = set("a")
  val doodah = set(1,2,3)
  val dash = set("X", "Y", "Z")
  val dashBig = set("X", "Y", "Z", "W")
  val dot2dash = arrow(dot, dash, "a"->"X", "b"->"Y")
  val dot2dashBadValues = arrow(dot, dash, "a"->"boojum", "b"->"heejum")
  val dot2dash_2 = arrow(dot, dash, "a"->"X", "b"->"Y")
  val dotSmall2dash = arrow(dotSmall, dash, "a"->"X", "b"->"Y")
  val dot2dashBig = arrow(dot, dashBig, "a"->"X", "b"->"Y")
  val dotBig2dash = arrow(dotBig, dash, "a"->"X", "b"->"Y")
  val doodah2dot = arrow(doodah, dot, 1->"a", 2->"b", 3->"a")

  describe("An arrow representing a morphism of finite sets") {
    it("should make accessible its source and target") {
      dot2dash.source shouldBe dot
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

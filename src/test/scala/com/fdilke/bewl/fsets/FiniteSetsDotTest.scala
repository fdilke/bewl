package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.FiniteSetsUtilities.{arrow, set}
import org.scalatest.{FunSpec, Matchers}
import Matchers._

class FiniteSetsDotTest extends FunSpec {
  describe("A dot representing a finite set") {
    it("should have partly sensible equality semantics") {
      val dot = set[String]("a", "b")
      val dash = set[String]("a", "b")
      val doodah = set[String]("a", "b", "c")

      dot shouldBe dot
      dot shouldBe dash
      dot should not be doodah
    }

    it("should have an identity arrow") {
      val dot = set[String]("a", "b")
      dot.identity shouldBe
        arrow[String, String](dot, dot, "a" -> "a", "b" -> "b")
    }
  }
}

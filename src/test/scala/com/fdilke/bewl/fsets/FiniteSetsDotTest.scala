package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities.{arrow, dot}
import org.scalatest.{FunSpec, Matchers}
import Matchers._

class FiniteSetsDotTest extends FunSpec {
  describe("A dot representing a finite set") {
    it("should have partly sensible equality semantics") {
      val aDot = dot[String]("a", "b")
      val dash = dot[String]("a", "b")
      val doodah = dot[String]("a", "b", "c")

      aDot shouldBe aDot
      aDot shouldBe dash
      aDot should not be doodah
    }

    it("should have an identity arrow") {
      val aDot = dot[String]("a", "b")
      aDot.identity shouldBe
        arrow[String, String](aDot, aDot, "a" -> "a", "b" -> "b")
    }
  }
}

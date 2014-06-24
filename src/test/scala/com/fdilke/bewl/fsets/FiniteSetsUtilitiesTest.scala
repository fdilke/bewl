package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities
import FiniteSetsUtilities._
import org.scalatest.{Matchers, FunSpec}
import Matchers._

// User: Felix Date: 28/05/2014 Time: 18:48

class FiniteSetsUtilitiesTest extends FunSpec {
  describe("cartesian products") {
    it("work in the trivial case") {
      cartesian[String](List()) shouldBe Seq(
        Seq()
      )
    }

    it("enumerate all sequences") {
      cartesian[Any](List(Seq(1, 2), Seq("a", "b"), Seq(true, false))).toList shouldBe Seq(
        Seq(1,"a",true), Seq(1,"a",false),
        Seq(1,"b",true), Seq(1,"b",false),
        Seq(2,"a",true), Seq(2,"a",false),
        Seq(2,"b",true), Seq(2,"b",false)
      )
    }
  }

  describe("allMaps()") {
    it("enumerates all maps between two sets") {
      allMaps(Seq(1, 2), Set("a", "b", "c")).map {
        f => Map(1->f(1), 2->f(2))
      } shouldBe Seq(
        Map(1->"a", 2->"a"), Map(1->"b", 2->"a"), Map(1->"c", 2->"a"),
        Map(1->"a", 2->"b"), Map(1->"b", 2->"b"), Map(1->"c", 2->"b"),
        Map(1->"a", 2->"c"), Map(1->"b", 2->"c"), Map(1->"c", 2->"c")
      )
    }
  }
}

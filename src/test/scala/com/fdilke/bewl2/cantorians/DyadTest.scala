package com.fdilke.bewl2.cantorians

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

class DyadTest extends AnyFunSpec {
  describe("Dyads") {
    it("can't be instantiated using 'new'") {
      "new Dyad(2)" shouldNot compile
    }
    it("can be instantiated using companion factory method") {
      Dyad(2).getClass shouldBe classOf[Dyad]
    }
  }
}

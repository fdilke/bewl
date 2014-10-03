package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.FiniteSetsUtilities.set
import org.scalatest.FunSpec
import org.scalatest.Matchers._

// User: Felix Date: 07/06/2014 Time: 12:28

class FiniteSetsExponentialTest extends FunSpec {
  describe("The exponential") {
    describe("has the correct number of values, all distinct") {
      val source = set('a, 'b, 'c)
      val target = set(1, 2, 3, 4)
      val expDot = target ^ source
      expDot.size shouldBe 64
    }
  }
}

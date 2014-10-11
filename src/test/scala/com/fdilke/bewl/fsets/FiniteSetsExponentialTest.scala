package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.DiagrammaticFiniteSetsUtilities.set
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class FiniteSetsExponentialTest extends FunSpec {
  describe("The exponential") {
    describe("has the correct number of values, all distinct") {
      val source = set('a, 'b, 'c)
      val target = set(1, 2, 3, 4)
      target ^ source should have ('size (64))
    }
  }
}

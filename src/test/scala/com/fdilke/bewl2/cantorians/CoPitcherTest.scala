package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.CoPitcher.isConstant
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CoPitcherTest extends AnyFunSpec {
  describe("Recasting Compact-to-Hausdorff functions as Catchers") {
    it("...we csn detect constants") {
      val nonConstant: Cantorian => Boolean =
        _.head

      isConstant[Cantorian, Boolean, Boolean](nonConstant) shouldBe false
      val aConstant: Cantorian => Boolean =
        _ => true

      isConstant[Cantorian, Boolean, Boolean](aConstant) shouldBe true
    }
  }
}

package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.CoPitcher.detectConstant
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class CoPitcherTest extends AnyFunSpec {
  private val all0CoP: Cantorian => Int =
    _ => 0
  private val all1CoP: Cantorian => Int =
    _ => 1
  private val headCoP: Cantorian => Int =
    c => if (c.head) 1 else 0
  private val notHeadCoP: Cantorian => Int =
    c => if (c.head) 0 else 1
  private val tCantorian: Cantorian =
    Cantorian.cycle(true)
  private val fCantorian: Cantorian =
    Cantorian.cycle(false)
  private val tfCantorian: Cantorian =
    Cantorian.cycle(true, false)
  private val ftCantorian: Cantorian =
    Cantorian.cycle(false, true)

  describe("Recasting Compact-to-Hausdorff functions as Catchers") {
    it("...we can detect constants") {
      detectConstant[Cantorian, Boolean, Int](headCoP) shouldBe None
      detectConstant[Cantorian, Boolean, Int](notHeadCoP) shouldBe None
      detectConstant[Cantorian, Boolean, Int](all0CoP) shouldBe Some(0)
      detectConstant[Cantorian, Boolean, Int](all1CoP) shouldBe Some(1)
    }
    it("preserves their action as functions") {
      implicit val catcherTude: Catcher[Cantorian => Int, Boolean, Int] =
        CoPitcher.functionAsCatcher[Cantorian, Boolean, Int]

      def applyFn(coc: Cantorian => Int)(cantorian: Cantorian): Int =
        Catcher.applyCatcher[Cantorian => Int, Cantorian, Boolean, Int](coc)(cantorian)

      for {
        coP <- Seq(all0CoP, all1CoP, headCoP, notHeadCoP)
        cantorian <- Seq(tCantorian, fCantorian, tfCantorian, ftCantorian)
      } applyFn(coP)(cantorian) shouldBe coP(cantorian)
    }
  }
}

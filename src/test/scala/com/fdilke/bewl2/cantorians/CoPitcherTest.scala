package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Cantorian._
import com.fdilke.bewl2.cantorians.CoPitcher.detectConstant
import com.fdilke.bewl2.topology.Compact.forAll
import com.fdilke.bewl2.topology.Hausdorff.equalH
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import Catcher._

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
      def applyFn(coc: Cantorian => Int)(cantorian: Cantorian): Int =
        Catcher.applyCatcher[CoPitcher[Cantorian, Boolean, Int], Cantorian, Boolean, Int](
          CoPitcher[Cantorian, Boolean, Int](coc)
        )(
          cantorian
        )

      for {
        coP <- Seq(all0CoP, all1CoP, headCoP, notHeadCoP)
        cantorian <- Seq(tCantorian, fCantorian, tfCantorian, ftCantorian)
      } applyFn(coP)(cantorian) shouldBe coP(cantorian)

      for {
        coP <- Seq(all0CoP, all1CoP, headCoP, notHeadCoP)
      } {
        equalH[Cantorian => Int](
          coP,
          cantorian => applyFn(coP)(cantorian)
        ) shouldBe true

        forAll[Cantorian] { cantorian =>
          applyFn(coP)(cantorian) == coP(cantorian)
        } shouldBe true
      }
    }
    it("can construct and deconstruct functions as catchers") {
      def coP(cantorian: Cantorian): Boolean =
        cantorian(2) ^ cantorian(5)
      def coP2(cantorian: Cantorian): Boolean =
        cantorian(3) ^ cantorian(6)

      equalH[CoPitcher[Cantorian, Boolean, Boolean]](
        Catcher.construct[CoPitcher[Cantorian, Boolean, Boolean], Boolean, Boolean](
          Left(false)
        ),
        CoPitcher[Cantorian, Boolean, Boolean](_ => false)
      ) shouldBe true

      equalH[CoPitcher[Cantorian, Boolean, Boolean]](
        Catcher.construct[CoPitcher[Cantorian, Boolean, Boolean], Boolean, Boolean](
          Right(_ => CoPitcher(coP))
        ),
        CoPitcher(coP2)
      ) shouldBe true
    }
  }
  describe("Co-pitchers") {
    it("can be constructed from a function and recast as other catchers") {
      val coC: Cantorian => Int =
        c => Set(0, 1, 2).map(c).size

      val coPitcher: CoPitcher[Cantorian, Boolean, Int] =
        CoPitcher(coC)

      coPitcher.as[Dyad[Int]] shouldBe
        Dyad(1, 2, 2, 2, 2, 2, 2, 1)

      equalH(
        coPitcher.function,
        coC
      ) shouldBe true
    }
    it("can be constructed from another catcher type and regarded as a dyad") {
      val tree: GroundedTree[Int] =
        GroundedTree(
          GroundedTree(1),
          GroundedTree(
            GroundedTree(2),
            GroundedTree(3)
          )
        )

      val coPitcher: CoPitcher[Cantorian, Boolean, Int] =
        CoPitcher(tree)

      equalH[Cantorian => Int](
        coPitcher.function,
        c => if (c.head) (if (c.tail.head) 3 else 2) else 1
      ) shouldBe true

      coPitcher.as[Dyad[Int]] shouldBe Dyad(
        1,
        2,
        1,
        3
      )
    }
    it("can be recast as any other catcher-of-boolean type") {
      val dyad: Dyad[Int] =
        Dyad(6, 5, 0, 2)
      CoPitcher[Cantorian, Boolean, Int](
        dyad(_)
      ).as[GroundedTree[Int]] shouldBe
        GroundedTree[Int](
          GroundedTree(
            GroundedTree(6),
            GroundedTree(0)
          ),
          GroundedTree(
            GroundedTree(5),
            GroundedTree(2)
          )
        )
    }
  }
}

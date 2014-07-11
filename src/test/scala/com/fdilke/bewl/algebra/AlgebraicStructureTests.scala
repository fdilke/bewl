package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicStructureTests extends FunSpec {
  import FiniteSets._

  def MagmaSignature = Set(Operator.*)
  case class Magma[X](dot: DOT[X], product: AlgebraicArrow[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MagmaSignature,
    ops = Map(Operator.* -> product),
    laws=Seq()
  )
  case class CommutativeMagma[X](dot: DOT[X], product: AlgebraicArrow[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MagmaSignature,
    ops = Map(Operator.* -> product),
    laws = Seq(Law.commutative(Operator.*))
  )

  describe("Algebraic structures") {
    it("can be constructed from arrows obeying laws") {

      val dot = set(0, 1)
      val product = AlgebraicArrow(dot A 2, FiniteSetsArrow[Power[Int], Int](
        dot ^ 2, dot, Map(
          (0, 0) -> 0, 
          (0, 1) -> 1, 
          (1, 0) -> 0, 
          (1, 1) -> 0
        )) // TODO: do this more smoothly

      Magma(dot, product).verify

      def invalidAlgebra = CommutativeMagma(dot, product)
      intercept[IllegalArgumentException] { invalidAlgebra.verify }
    }
  }
}

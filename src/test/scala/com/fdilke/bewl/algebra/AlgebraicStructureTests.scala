package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicStructureTests extends FunSpec {
  import FiniteSets._

  def MagmaSignature = Set(AbstractOperator.*)
  case class Magma[X](dot: DOT[X], product: AlgebraicOperator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MagmaSignature,
    ops = Map(AbstractOperator.* -> product),
    laws=Seq()
  )
  case class CommutativeMagma[X](dot: DOT[X], product: AlgebraicOperator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MagmaSignature,
    ops = Map(AbstractOperator.* -> product),
    laws = Seq(Law.commutative(AbstractOperator.*))
  )

  describe("Algebraic structures") {
    it("can be constructed from arrows obeying laws") {
      val dot = set(0, 1)
      val product = binaryOperator(dot,
          (0, 0) -> 0,
          (0, 1) -> 1, 
          (1, 0) -> 0, 
          (1, 1) -> 0
        )

      Magma[Int](dot, product).verify

      def invalidAlgebra = CommutativeMagma[Int](dot, product)
      intercept[IllegalArgumentException] { invalidAlgebra.verify }
    }
  }
}

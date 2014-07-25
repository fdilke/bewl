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
    operatorMap = Map(AbstractOperator.* -> product)
  )
  case class CommutativeMagma[X](dot: DOT[X], product: AlgebraicOperator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MagmaSignature,
    operatorMap = Map(AbstractOperator.* -> product),
    Law.commutative(AbstractOperator.*)
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
      intercept[IllegalArgumentException] { invalidAlgebra.verify }.
        getMessage shouldBe "Commutative law for operator *"
    }

    it("are defined for monoids") {
      def i = 'i
      def x = 'x
      def y = 'y
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val product = binaryOperator(dot,
        (i, i) -> i,    (i, x) -> x,    (i, y) -> y,
        (x, i) -> x,    (x, x) -> x,    (x, y) -> x,
        (y, i) -> y,    (y, x) -> y,    (y, y) -> y
      )
      Monoid[Symbol](dot, unit, product).verify
    }
  }
}

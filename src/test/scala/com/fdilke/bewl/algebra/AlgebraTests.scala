package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraTests extends FunSpec {
  import FiniteSets._

  def MagmaSignature = Set(AbstractOperator.*)
  case class Magma[X](dot: DOT[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MagmaSignature,
    operatorMap = Map(AbstractOperator.* -> product)
  )
  case class CommutativeMagma[X](dot: DOT[X], product: Operator[X]) extends AlgebraicStructure[X] (
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
      intercept[IllegalArgumentException] {
        invalidAlgebra.verify
      }.
        getMessage shouldBe "Commutative law for operator *"
    }
  }

  describe("Monoids") {
    it("can be defined with an appropriate unit and multiplication") {
      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      Monoid[Symbol](dot, unit, product).verify
    }

    it("must have a left unit element") {
      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> i, (i, y) -> i,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        Monoid[Symbol](dot, unit, product).verify
      }.
        getMessage shouldBe "Left unit law for operator * with unit 1"
    }

    it("must have a right unit element") {
      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> i, (x, x) -> x, (x, y) -> y,
        (y, i) -> i, (y, x) -> x, (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        Monoid[Symbol](dot, unit, product).verify
      }.
        getMessage shouldBe "Right unit law for operator * with unit 1"
    }

    it("must be associative") {
      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> y, (x, y) -> y,
        (y, i) -> y, (y, x) -> x, (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        Monoid[Symbol](dot, unit, product).verify
      }.
        getMessage shouldBe "Associative law for operator *"
    }
  }

  describe("Groups") {
    it("can be defined with an appropriate unit, multiplication and inverse") {
      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val inverse = unaryOperator(dot,
        i -> i, x -> y, y -> x
      )
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> y, (x, y) -> i,
        (y, i) -> y, (y, x) -> i, (y, y) -> x
      )
      Group[Symbol](dot, unit, product, inverse).verify
    }

    it("must have inverses for every element") {
      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val inverse = unaryOperator(dot,
        i -> i, x -> y, y -> x
      )
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        Group[Symbol](dot, unit, product, inverse).verify
      }.
        getMessage shouldBe "Left inverse law for operator * with inverse inv and unit 1"
    }
  }
}

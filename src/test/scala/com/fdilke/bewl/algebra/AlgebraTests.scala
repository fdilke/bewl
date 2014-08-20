package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraTests extends FunSpec {
  import FiniteSets._
  import AbstractOperator._

  def Magmas = new AlgebraicTheory(Seq(*))
  case class Magma[X](dot: DOT[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(* -> product),
    theory = Magmas
  )
  def CommutativeMagmas = new AlgebraicTheory(Seq(*),
    commutative(*)
  )
  case class CommutativeMagma[X](dot: DOT[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(* -> product),
    theory = CommutativeMagmas
  )

  describe("Algebraic theories") {
    it("must define all the operations used in their laws") {
      intercept[IllegalArgumentException] {
        new AlgebraicTheory(Seq(), commutative(*))
      }.getMessage shouldBe "Law uses undefined operator *"
    }
    // Note it's OK to define operations but not make use of them in laws - e.g. magmas
  }

  describe("Algebraic structures") {
    it("can be constructed from arrows obeying laws specified by a theory") {
      val dot = set(0, 1)
      val product = binaryOperator(dot,
        (0, 0) -> 0,
        (0, 1) -> 1,
        (1, 0) -> 0,
        (1, 1) -> 0
      )

      Magma(dot, product).verify

      def invalidAlgebra = CommutativeMagma(dot, product)
      intercept[IllegalArgumentException] {
        invalidAlgebra.verify
      }.
        getMessage shouldBe "Commutative law for *"
    }

    ignore("automatically verify the arities of supplied operations - can't currently, as they are just arrows") {
      val (o, x) = ('o, 'x)
      val dot = set(o, x)
      val flip = unaryOperator(dot,
        o -> x, x -> o
      )

      def invalidAlgebra = Magma(dot, flip)
        intercept[IllegalArgumentException] {
        invalidAlgebra.verify
      }.
        getMessage shouldBe "Operator * should have arity 2"
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
      Monoid(dot, unit, product).verify
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
        Monoid(dot, unit, product).verify
      }.
        getMessage shouldBe "Left unit law for * with unit 1"
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
        Monoid(dot, unit, product).verify
      }.
        getMessage shouldBe "Right unit law for * with unit 1"
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
        Monoid(dot, unit, product).verify
      }.
        getMessage shouldBe "Associative law for *"
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
      Group(dot, unit, product, inverse).verify
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
        Group(dot, unit, product, inverse).verify
      }.
        getMessage shouldBe "Left inverse law for * with inverse invert and unit 1"
    }
  }

  describe("Abelian groups") {
    it("can be defined with an appropriate zero, addition and subtraction") {
      val (o, x, y) = ('o, 'x, 'y)
      val dot = set(o, x, y)
      val zero = nullaryOperator(dot, o)
      val inverse = unaryOperator(dot,
        o -> o, x -> y, y -> x
      )
      val sum = binaryOperator(dot,
        (o, o) -> o, (o, x) -> x, (o, y) -> y,
        (x, o) -> x, (x, x) -> y, (x, y) -> o,
        (y, o) -> y, (y, x) -> o, (y, y) -> x
      )
      AdditiveAbelianGroup(dot, zero, sum, inverse).verify
    }

    it("must have inverses for every element") {
      val (o, x, y) = ('o, 'x, 'y)
      val dot = set(o, x, y)
      val zero = nullaryOperator(dot, o)
      val negate = unaryOperator(dot,
        o -> o, x -> y, y -> x
      )
      val sum = binaryOperator(dot,
        (o, o) -> o, (o, x) -> x, (o, y) -> y,
        (x, o) -> x, (x, x) -> x, (x, y) -> y,
        (y, o) -> y, (y, x) -> y, (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        AdditiveAbelianGroup(dot, zero, sum, negate).verify
      }.
        getMessage shouldBe "Left inverse law for + with inverse negate and unit 0"
    }

    it("must be commutative") {
      val (i, x, y, z, r, s) = ('i, 'x, 'y, 'z, 'r, 's)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      val inverse = unaryOperator(dot,
        i -> i, x -> x, y -> y, z -> z, r -> s, s -> r
      )
      val product = binaryOperator(dot,
        (i, i) -> i, (i, x) -> x, (i, y) -> y, (i, z) -> z, (i, r) -> r, (i, s) -> s,
        (x, i) -> x, (x, x) -> i, (x, y) -> r, (x, z) -> s, (x, r) -> y, (x, s) -> z,
        (y, i) -> y, (y, x) -> s, (y, y) -> i, (y, z) -> r, (y, r) -> z, (y, s) -> x,
        (z, i) -> z, (z, x) -> r, (z, y) -> s, (z, z) -> i, (z, r) -> x, (z, s) -> y,
        (r, i) -> r, (r, x) -> z, (r, y) -> x, (r, z) -> y, (r, r) -> s, (r, s) -> i,
        (s, i) -> s, (s, x) -> y, (s, y) -> z, (s, z) -> x, (s, r) -> i, (s, s) -> r
      )
      intercept[IllegalArgumentException] {
        AdditiveAbelianGroup(dot, unit, product, inverse).verify
      }.
        getMessage shouldBe "Commutative law for +"
    }
  }

  describe("Rings") {
    it("can be defined with an appropriate zero, addition and subtraction, and multiplication") {
      val dot = set(0 until 7 :_*)
      val zero = nullaryOperator(dot, 0)
      val one = nullaryOperator(dot, 1)
      val negate = unaryOperator(dot,
        { x:Int => (7-x) % 7 }
      )

      val sum = binaryOperator(dot,
        { (x:Int, y:Int) => (x + y) % 7 })
      val product = binaryOperator(dot,
        { (x:Int, y:Int) => (x * y) % 7 })

      Ring(dot, zero, one, sum, negate, product).verify
    }
  }

  describe("Lattices") {
    it("can be defined with an appropriate 0,1,^,v") {
      val dot = set(0 until 8 :_*)
      val zero = nullaryOperator(dot, 0)
      val one = nullaryOperator(dot, 7)

      val meet = binaryOperator(dot,
      { (x:Int, y:Int) => x & y })
      val join = binaryOperator(dot,
      { (x:Int, y:Int) => x | y })

      Lattice(dot, zero, one, meet, join).verify
    }
  }

  describe("Heyting algebras") {
    it("can be defined with an appropriate 0,1,^,v,->") {
      val dot = set(0 until 8 :_*)
      val zero = nullaryOperator(dot, 0)
      val one = nullaryOperator(dot, 7)

      val meet = binaryOperator(dot,
      { (x:Int, y:Int) => x & y })
      val join = binaryOperator(dot,
      { (x:Int, y:Int) => x | y })
      val imply = binaryOperator(dot,
      { (x:Int, y:Int) => (x ^ 7) | y  })

      HeytingAlgebra(dot, zero, one, meet, join, imply).verify
    }
  }
}

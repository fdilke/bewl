package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraTests extends FunSpec {

  private val topos = com.fdilke.bewl.fsets.FiniteSets
  import topos.StandardTermsAndOperators._

  describe("Simple and compound terms") {
    it("can describe their own free variables") {
      α.freeVariables shouldBe Seq(α)
      (α * β).freeVariables shouldBe Seq(α, β)
    }
  }

  private val unstructuredSets = AlgebraicTheory()()()

  describe("An evaluation context") {
    it("for no terms can evaluate constants, not veriables") {
      val carrier = dot[Boolean](true, false)
      val theO = makeNullaryOperator(carrier, false)
      val pointedSets = AlgebraicTheory(O)()()
      val algebra = new pointedSets.Algebra[Boolean](carrier)(O := theO)
      val context = algebra.EvaluationContext[Boolean](Seq())
      intercept[IllegalArgumentException] {
        context.evaluate(α)
      }
      intercept[IllegalArgumentException] {
        context.evaluateScalar(II)
      }
      context.evaluate(O) shouldBe theO
    }

    it("for one term over an empty theory is just a uniproduct") {
      val carrier = dot[Boolean](true, false)
      val algebra = new unstructuredSets.Algebra[Boolean](carrier)()
      val context = algebra.EvaluationContext[Boolean](Seq(α))
      context.evaluate(α) should have (
        'source(context.root),
        'target(carrier),
        'iso(true)
      )
    }

    it("for two terms over an empty theory is just a biproduct") {
      val carrier = dot[Boolean](true, false)
      val algebra = new unstructuredSets.Algebra[Boolean](carrier)()
      val context = algebra.EvaluationContext[Boolean](Seq(α, β))
      context.evaluate(α) should have (
        'source(context.root),
        'target(carrier)
      )
      context.evaluate(β) should have (
        'source(context.root),
        'target(carrier)
      )
      context.evaluate(α) x context.evaluate(β) shouldBe 'iso
    }

    it("for one term can evaluate constants") {
      val carrier = dot[Boolean](true, false)
      val theO = makeNullaryOperator(carrier, false)

      val pointedSets = AlgebraicTheory(O)()()
      val algebra = new pointedSets.Algebra[Boolean](carrier)(O := theO)
      val context = algebra.EvaluationContext[Boolean](Seq(α))
      context.evaluate(O) shouldBe (
          theO o context.root.toI
      )
    }

    it("can evaluate compound terms with unary operators") {
      val carrier = dot[Int](0, 1, 2)
      val theO = makeNullaryOperator(carrier, 0)
      val twiddle = makeUnaryOperator(carrier,
        0 -> 0,
        1 -> 2,
        2 -> 1
      )

      val pointedSetsWithOp = AlgebraicTheory(O)($minus)()
      val algebra = new pointedSetsWithOp.Algebra[Int](carrier)(O := theO, $minus := twiddle)
      val context = algebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretα = context.evaluate(α)

      interpretα should not be interpretO
      context.evaluate(O) shouldBe interpretO
      val minusO = -O
      context.evaluate(-O) shouldBe interpretO
      context.evaluate(-(-α)) shouldBe interpretα
    }

    it("can evaluate compound terms with binary operators") {
      val carrier = dot[String]("unit", "x")
      val theO = makeNullaryOperator(carrier, "unit")
      val plus = makeBinaryOperator(carrier,
        ("unit", "unit") -> "unit",
        ("x", "unit") -> "x",
        ("unit", "x") -> "x",
        ("x", "x") -> "x"
      )

      val pointedMagmas = AlgebraicTheory(O)($plus)()
      val algebra = new pointedMagmas.Algebra[String](carrier)(O := theO, $plus := plus)
      val context = algebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretα = context.evaluate(α)

      interpretα should not be interpretO
      context.evaluate(O) shouldBe interpretO
      context.evaluate(O + O) shouldBe interpretO
      context.evaluate(O + α) shouldBe interpretα
      context.evaluate(α + O) shouldBe interpretα
      context.evaluate(α + O) shouldBe interpretα
    }

    it("can evaluate compound terms with mixed binary operators") {
      val scalars = dot[Int](0, 1, 2)

      val carrier = dot[String]("o", "i")
      val theO = makeNullaryOperator(carrier, "o")
      val scalar1 = makeNullaryOperator(scalars, 2)
      val table = Map(
        ("o", 0) -> "o",
        ("i", 0) -> "o",
        ("o", 1) -> "i",
        ("i", 1) -> "i",
        ("o", 2) -> "i",
        ("i", 2) -> "o"
      )
      val rightMultiply = bifunctionAsBiArrow(carrier, scalars, carrier)(
        Function untupled table
      )

      val pointedWeakActs = AlgebraicTheoryWithScalars(scalars)(O, II)(II := scalar1)(**)()
      val minimalAlgebra = new pointedWeakActs.Algebra[String](carrier)(
        O := theO, ** := rightMultiply
      )
      val context = minimalAlgebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretI = makeNullaryOperator(carrier, "i") o context.root.toI

      context.evaluate(O ** II) shouldBe interpretI
      context.evaluate((α ** II) ** II) shouldBe context.evaluate(α)
    }
  }

  describe("Algebraic theories") {
    it("can encapsulate commutative magmas") {
      val commutativeMagmas = AlgebraicTheory()(*)(α * β := β * α)
      case class CommutativeMagma[T <: ~](
        carrier: DOT[T], op: BinaryOp[T]
      ) extends commutativeMagmas.Algebra[T](carrier)(* := op)

      val carrier = dot(true, false)
      val commutativeOp = bifunctionAsBiArrow(carrier) { _ & _}
      val nonCommutativeOp = bifunctionAsBiArrow(carrier) { _ & !_ }

     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean](carrier)().sanityTest
     }
     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean](carrier)($plus := commutativeOp).sanityTest
     }

      CommutativeMagma(carrier, commutativeOp).sanityTest
      intercept[IllegalArgumentException] {
        CommutativeMagma(carrier, nonCommutativeOp).sanityTest
      }
    }

    it("can validate morphisms between their algebras") {
      val carrierStrings = dot[String]("+", "-")
      val minusStrings = makeUnaryOperator(carrierStrings,
        "+" -> "-",
        "-" -> "+"
      )
      val carrierInts = dot[Int](1, -1)
      val minusInts = makeUnaryOperator(carrierInts,
        1 -> -1,
        -1 -> 1
      )
      val wrongSource = dot[String]("*")
      val wrongTarget = dot[Int](0)

      val setsWithInvolution = AlgebraicTheory()($minus)(-(-α) := α)
      val algebraStrings = new setsWithInvolution.Algebra[String](carrierStrings)($minus := minusStrings)
      val algebraInts = new setsWithInvolution.Algebra[Int](carrierInts)($minus := minusInts)
      algebraStrings.sanityTest
      algebraInts.sanityTest

      val morphism = arrow(carrierStrings, carrierInts, "+" -> 1, "-" -> -1)
      val morphismWithWrongSource = arrow(wrongSource, carrierInts, "*" -> 1)
      morphismWithWrongSource.sanityTest
      val morphismWithWrongTarget = arrow(carrierStrings, wrongTarget, "+" -> 0, "-" -> 0)
      morphismWithWrongTarget.sanityTest
      setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphism) shouldBe true
      intercept[IllegalArgumentException] {
        setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphismWithWrongSource);
      }
      intercept[IllegalArgumentException] {
        setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphismWithWrongTarget);
      }

      val notAMorphism = arrow(carrierStrings, carrierInts, "+" -> 1, "-" -> 1)
      setsWithInvolution.isMorphism(algebraStrings, algebraInts, notAMorphism) shouldBe false
    }
  }
}

package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicMachineryTest extends FunSpec {

  private val topos = com.fdilke.bewl.fsets.FiniteSets
  import topos.StandardTermsAndOperators._

  describe("Simple and compound terms") {
    it("can describe their own free variables") {
      α.freeVariables shouldBe Seq(α)
      (α * β).freeVariables shouldBe Seq(α, β)
    }
  }

  private val unstructuredSets = AlgebraicTheory()()

  describe("An evaluation context") {
    it("for no terms can evaluate constants, not variables") {
      val carrier = dot[Boolean](true, false)
      val theO = makeNullaryOperator(carrier, false)
      val pointedSets = AlgebraicTheory(o)()
      val algebra = new pointedSets.Algebra[Boolean](carrier)(o := theO)
      val context = algebra.EvaluationContext[Boolean](Seq())
      intercept[IllegalArgumentException] {
        context.evaluate(α)
      }
      intercept[IllegalArgumentException] {
        context.evaluateScalar(II)
      }
      context.evaluate(o) shouldBe theO
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

      val pointedSets = AlgebraicTheory(o)()
      val algebra = new pointedSets.Algebra[Boolean](carrier)(o := theO)
      val context = algebra.EvaluationContext[Boolean](Seq(α))
      context.evaluate(o) shouldBe (
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

      val pointedSetsWithOp = AlgebraicTheory(o, $minus)()
      val algebra = new pointedSetsWithOp.Algebra[Int](carrier)(o := theO, $minus := twiddle)
      val context = algebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretα = context.evaluate(α)

      interpretα should not be interpretO
      context.evaluate(o) shouldBe interpretO
      val minusO = -o
      context.evaluate(-o) shouldBe interpretO
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

      val pointedMagmas = AlgebraicTheory(o, $plus)()
      val algebra = new pointedMagmas.Algebra[String](carrier)(o := theO, $plus := plus)
      val context = algebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretα = context.evaluate(α)

      interpretα should not be interpretO
      context.evaluate(o) shouldBe interpretO
      context.evaluate(o + o) shouldBe interpretO
      context.evaluate(o + α) shouldBe interpretα
      context.evaluate(α + o) shouldBe interpretα
      context.evaluate(α + o) shouldBe interpretα
    }

    it("can evaluate compound terms with mixed binary operators") {
      val scalars = dot[Int](0, 1, 2)

      val carrier = dot[String]("o", "i")
      val theO = makeNullaryOperator(carrier, "o")
      val scalar1 = makeNullaryOperator(scalars, 2)
      val rightMultiply = bifunctionAsBiArrow(carrier, scalars, carrier)(
        Function untupled Map(
          ("o", 0) -> "o",
          ("i", 0) -> "o",
          ("o", 1) -> "i",
          ("i", 1) -> "i",
          ("o", 2) -> "i",
          ("i", 2) -> "o"
        )
      )

      val pointedWeakActs = AlgebraicTheoryWithScalars(scalars)(II := scalar1)(o, II, **)()
      val minimalAlgebra = new pointedWeakActs.Algebra[String](carrier)(
        o := theO, ** := rightMultiply
      )
      val context = minimalAlgebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretI = makeNullaryOperator(carrier, "i") o context.root.toI

      context.evaluate(o ** II) shouldBe interpretI
      context.evaluate((α ** II) ** II) shouldBe context.evaluate(α)
    }

    it("can do operations on scalars") {
      val scalars = dot(0, 1, 2)
      val scalar1 = makeNullaryOperator(scalars, 1)
      val scalar2 = makeNullaryOperator(scalars, 2)
      val weakActsReferencingAMonoid = AlgebraicTheoryWithScalars(scalars)(II := scalar1)(**, ***)()
      val act = dot('a)
      val algebra = new weakActsReferencingAMonoid.Algebra[Symbol](act)(
        ** := (act x scalars).biArrow(act) { (_, _) => 'a },
        *** := bifunctionAsBiArrow(scalars) {
          (x, y) => (x + y) % 3
        }
      )

      algebra.EvaluationContext(Seq()).evaluateScalar(
        II *** II
      ) shouldBe scalar2

      algebra.EvaluationContext(Seq(α)).evaluate(
        α
      ) shouldBe (act x I).π0

      algebra.EvaluationContext(Seq(α)).evaluate(
        α ** II
      ) shouldBe (act x I).π0

      algebra.EvaluationContext(Seq(Ψ)).evaluateScalar(
        Ψ
      ) shouldBe (scalars x I).π0

      algebra.EvaluationContext(Seq(Ψ)).evaluateScalar(
        ((Ψ *** II) *** II) *** II
      ) shouldBe (scalars x I).π0
    }
  }

  describe("Algebraic theories") {
    it("can encapsulate commutative magmas") {
      val commutativeMagmas = AlgebraicTheory(*)(α * β := β * α)
      case class CommutativeMagma[T <: ~](
        override val carrier: DOT[T],
        op: BinaryOp[T]
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

    it("can validate morphisms preserving unary operations") {
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

      val setsWithInvolution = AlgebraicTheory($minus)(-(-α) := α)
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

    it("can validate morphisms preserving constants") {
      val carrierInts = dot[Int](0, 1, 2)
      val pointInts = makeNullaryOperator(carrierInts, 0);
      val carrierStrings = dot[String]("samson", "delilah")
      val pointStrings = makeNullaryOperator(carrierStrings, "delilah");

      val pointedSets = AlgebraicTheory(o)()
      val algebraStrings = new pointedSets.Algebra[String](carrierStrings)(o := pointStrings)
      val algebraInts = new pointedSets.Algebra[Int](carrierInts)(o := pointInts)

      val morphism = arrow(carrierStrings, carrierInts, "samson" -> 1, "delilah" -> 0)
      val notAMorphism = arrow(carrierStrings, carrierInts, "samson" -> 1, "delilah" -> 1)

      pointedSets.isMorphism(algebraStrings, algebraInts, morphism) shouldBe true
      pointedSets.isMorphism(algebraStrings, algebraInts, notAMorphism) shouldBe false
    }

    it("can validate morphisms preserving binary operations") {
      val carrier = dot[Int](0, 1, 2, 3)
      val multiplication = bifunctionAsBiArrow(carrier) {
        (x, y) => (x + y) % 4
      }

      val magmas = AlgebraicTheory(*)()
      val algebra = new magmas.Algebra[Int](carrier)(* := multiplication)

      val morphism = arrow(carrier, carrier, 0 -> 0, 1 -> 2, 2 -> 0, 3 -> 2)
      val notAMorphism = arrow(carrier, carrier, 0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)

      magmas.isMorphism(algebra, algebra, morphism) shouldBe true
      magmas.isMorphism(algebra, algebra, notAMorphism) shouldBe false
    }

    it("can validate morphisms preserving mixed operations with scalars") {
      val scalars = dot[Symbol]('q)
      val pointScalar = makeNullaryOperator(scalars, 'q)
      val carrier = dot[Boolean](true, false)
      val multiplication = bifunctionAsBiArrow(carrier, scalars, carrier) {
        Function untupled Map(
          (true, 'q) -> false,
          (false, 'q) -> true
        )
      }

      val weakActsOverAPointedMagma = AlgebraicTheoryWithScalars(scalars)(II := pointScalar)(II, **)()
      val algebra = new weakActsOverAPointedMagma.Algebra[Boolean](carrier)(** := multiplication)
      algebra.sanityTest

      val morphism = arrow(carrier, carrier, true -> false, false -> true)
      val notAMorphism = arrow(carrier, carrier, true -> true, false -> true)

      weakActsOverAPointedMagma.isMorphism(algebra, algebra, morphism) shouldBe true
      weakActsOverAPointedMagma.isMorphism(algebra, algebra, notAMorphism) shouldBe false
    }

    it("support named laws with error reporting when they fail") {
      import NamedLaws._
      val carrier = dot[Int](1, -1)
      val minusGood = makeUnaryOperator(carrier,
        1 -> -1,
        -1 -> 1
      )
      val minusBad = makeUnaryOperator(carrier,
        1 -> 1,
        -1 -> 1
      )

      val setsWithInvolution = AlgebraicTheory($minus)(
        "involutive" law (-(-α) := α)
      )
      new setsWithInvolution.Algebra[Int](carrier)($minus := minusGood).sanityTest
      intercept[IllegalArgumentException] {
        new setsWithInvolution.Algebra[Int](carrier)($minus := minusBad).sanityTest
      }.getMessage should include("involutive")
    }

    it("can verify additional laws once an algebra is constructed") {
      import NamedLaws._
      val carrier = dot[Int](1, -1)
      val minus = makeUnaryOperator(carrier,
        1 -> -1,
        -1 -> 1
      )

      val setsWithInvolution = AlgebraicTheory($minus)(
        "involutive" law (-(-α) := α)
      )
      val algebra = new setsWithInvolution.Algebra[Int](carrier)($minus := minus)
      algebra.sanityTest

      algebra.satisfies(
        -α := α
      ) shouldBe false
      algebra.satisfies(
        -α := -(-(-α))
      ) shouldBe true
    }
  }
}

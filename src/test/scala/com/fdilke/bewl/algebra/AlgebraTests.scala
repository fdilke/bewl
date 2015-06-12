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
  describe("An evaluation context") {
    it("can evaluate terms, mapping types correctly") {
      val carrier = dot(true, false)
      val context = new EvaluationContext[Boolean](carrier, Seq(α, β))
      context.evaluate(α) should have (
        'source(context.root),
        'target(carrier)
      )
      context.evaluate(β) should have (
        'source(context.root),
        'target(carrier)
      )
//      context.evaluate(α) x context.evaluate(β) shouldBe context.root.identity
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
        new commutativeMagmas.Algebra[Boolean](carrier)(⊕ := commutativeOp).sanityTest
     }

      CommutativeMagma(carrier, commutativeOp).sanityTest
// TODO: make this work
//      intercept[IllegalArgumentException] {
        CommutativeMagma(carrier, nonCommutativeOp).sanityTest
//      }
    }
  }


/*
  private val leftUnitLaw = Law("not a left unit", (x: Variable[Principal]) =>
    multiply(unit, x) ::== x
  )

  private val magmasWith1 = AlgebraicTheory(
    operators = Seq(unit, multiply),
    laws = Seq(leftUnitLaw)
  )

  private val magmasWith1WithUnaryOperator = AlgebraicTheory(
    operators = Seq(unit, multiply, unaryOperator),
    laws = Seq(leftUnitLaw)
  )

  //  describe("Arities") {
  //    it("have a computed type") {
  //      Arity().calcType.TYPE
  //    }
  //  }

  describe("Multiproducts") {
    it("are computed sensibly for a product of 0") {
      val multiProduct = MultiProduct()
      multiProduct.root shouldBe I
      multiProduct.projections shouldBe 'empty
    }

    it("are computed sensibly for a product of 1") {
      val star = dot(1, 2)
      val multiProduct = MultiProduct(star)
      multiProduct.root shouldBe star
      multiProduct.projections shouldBe Seq(star.identity)
    }

    //    it("are computed sensibly for a product of 2") {
    //      val star1 = makeStar(1, 2, 3)
    //      val star2 = makeStar(true, false)
    //      val multiProduct = MultiProduct(star1, star2)
    //      val expectedProduct = star1 x star2
    //      multiProduct.root shouldBe expectedProduct
    //      multiProduct.projections shouldBe Seq(expectedProduct.π0, expectedProduct.π1)
    //    }
  }

  describe("Abstract operators") {

    it("are typed so that they can be combined only with matching arities and return values") {
      inContextOf(imports = Seq("com.fdilke.bewl.fsets.FiniteSets.AbstractOp._")) {
        "multiply(unit, unit)" should compile
        "rightScalarMultiply(unit, unitRightScalar)" should compile

        "multiply(unit)" should not(compile)
        "multiply(unit, unitRightScalar)" should not(compile)
        "rightScalarMultiply(unit, unit)" should not(compile)
      }
    }

    it("can be used as keys to a map of operator assignments") {
      val carrier = dot(true, false)
      val scalars = dot(1, 2, 3)
      val op0 = makeNullaryOperator(carrier, true)
      val op1 = carrier.identity
      val op2 = bifunctionAsBiArrow(carrier) {
        _ & _
      }
      val opRSM = bifunctionAsBiArrow(carrier, scalars, carrier) { (b, n) => if (b) (n > 1) else true}
      val assignments = new OpAssignments[Boolean](unit := op0, unaryOperator := op1, multiply := op2, rightScalarMultiply := opRSM)
      assignments.lookup(unit) shouldBe op0
      assignments.lookup(unaryOperator) shouldBe op1
      assignments.lookup(multiply) shouldBe op2
      assignments.lookup(rightScalarMultiply) shouldBe opRSM
    }

    it("can construct terms which can be evaluated in a root context") {

      val carrier = dot(0, 1, 2, 3)
      val product = bifunctionAsBiArrow(carrier) { (x, y) => (x + y) % 4}
      def constant(i: Int) = makeNullaryOperator(carrier, i)
      val myUnit = constant(0)
      val myUnaryOperator = carrier(carrier) { x => 3 - x}
      val zMod4 = magmasWith1WithUnaryOperator(carrier, unit := myUnit, multiply := product, unaryOperator := myUnaryOperator)

      val context0 = RootContext.forNullary(zMod4)

      context0.evaluate(unit) shouldBe (myUnit o context0.root.toI)
      context0.evaluate(unaryOperator(unit)) shouldBe (constant(3) o context0.root.toI)
      context0.evaluate(multiply(unaryOperator(unit), unaryOperator(unit))) shouldBe (constant(2) o context0.root.toI)

      // TODO: root contexts with a nonempty arity ... ?

      val context1 = RootContext.forUnary(zMod4)
      // TODO: make this work!
      if (false)
        context1.evaluate(unaryOperator(context1.variables(0))) shouldBe myUnaryOperator
    }
  }

  describe("Star patterns") {
    it("can be defined ") {

    }
  }

  describe("Algebraic theories") {

    it("can be defined and used to create instances") {
      // TODO: aspirational: should eventually be able to do this, and actions: test drive other machinery first
      //        val monoids = new AlgebraicTheory(
      //          operators = Set(AbstractOp.unit, AbstractOp.multiply),
      //          laws = Seq(
      //            Law.leftUnit(AbstractOp.unit, AbstractOp.multiply),
      //            Law.rightUnit(AbstractOp.unit, AbstractOp.multiply),
      //            Law.associative(AbstractOp.multiply)
      //          ))
      //        val (i, x, y) = ('i, 'x, 'y)
      //        val carrier = makeStar(i, x, y)
      //        val unit = makeNullaryOperator(carrier, i)
      //        val product = makeBinaryOperator(carrier,
      //          (i, i) -> i, (i, x) -> x, (i, y) -> y,
      //          (x, i) -> x, (x, x) -> x, (x, y) -> x,
      //          (y, i) -> y, (y, x) -> y, (y, y) -> y
      //        )
      //        monoids[Symbol](carrier, AbstractOp.unit := unit, AbstractOp.multiply := product).sanityTest
    }

  }

  describe("Algebraic laws") {
    it("can be verified for particular algebras") {
      val carrier = dot(1, -1)
      val product = makeBinaryOperator(carrier,
        (1, 1) -> 1, (1, -1) -> -1, (-1, 1) -> -1, (-1, -1) -> 1
      )
      val goodMagmaWith1 = magmasWith1(carrier, unit := makeNullaryOperator(carrier, 1), multiply := product)
      goodMagmaWith1.sanityTest

      if (false) {
        // TODO: fix
        val badMagmaWith1 = magmasWith1(carrier, unit := makeNullaryOperator(carrier, -1), multiply := product)
        intercept[IllegalArgumentException] {
          badMagmaWith1.sanityTest
        }.getMessage shouldBe "Left unit law failed"
      }
    }

    it("can be verified in the context of an algebraic theory, for a specified algebra") {
    }
  }
*/  
}

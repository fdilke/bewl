package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.NativeFiniteSets
import com.fdilke.bewl.fsets.NativeFiniteSets.AbstractOp._
import com.fdilke.bewl.fsets.NativeFiniteSets._
import com.fdilke.bewl.fsets.NativeFiniteSetsUtilities._
import com.fdilke.bewl.testutil.RunTimeCompilation
import com.fdilke.bewl.topos.StarTag.Principal
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraTests extends FunSpec with RunTimeCompilation {

  private val leftUnitLaw = Law("not a left unit", (x : Variable[Principal]) =>
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
      val star = makeStar(1, 2)
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
      inContextOf(imports = Seq("com.fdilke.bewl.fsets.NativeFiniteSets.AbstractOp._")) {
        "multiply(unit, unit)" should compile
        "rightScalarMultiply(unit, unitRightScalar)" should compile

        "multiply(unit)" should not (compile)
        "multiply(unit, unitRightScalar)" should not (compile)
        "rightScalarMultiply(unit, unit)" should not (compile)
      }
    }

    it("can be used as keys to a map of operator assignments") {
      val carrier = makeStar(true, false)
      val scalars = makeStar(1, 2, 3)
      val op0 = makeNullaryOperator(carrier, true)
      val op1 = carrier.identity
      val op2 = bifunctionAsBiQuiver(carrier) { _ & _ }
      val opRSM = bifunctionAsBiQuiver(carrier, scalars, carrier) { (b, n) => if (b) (n > 1) else true }
      val assignments = new OpAssignments[Boolean](unit := op0, unaryOperator := op1, multiply := op2, rightScalarMultiply := opRSM)
      assignments.lookup(unit) shouldBe op0
      assignments.lookup(unaryOperator) shouldBe op1
      assignments.lookup(multiply) shouldBe op2
      assignments.lookup(rightScalarMultiply) shouldBe opRSM
    }

    it("can construct terms which can be evaluated in a root context") {

      val carrier = makeStar(0, 1, 2, 3)
      val product = bifunctionAsBiQuiver(carrier) { (x, y) => (x + y) % 4 }
      def constant(i: Int) = makeNullaryOperator(carrier, i)
      val myUnit = constant(0)
      val myUnaryOperator = carrier(carrier) { x => 3 - x }
      val zMod4 = magmasWith1WithUnaryOperator(carrier, unit := myUnit, multiply := product, unaryOperator := myUnaryOperator)

      val context0 = RootContext(zMod4, Arity())

      context0.evaluate(unit) shouldBe (myUnit o context0.root.toI)
      context0.evaluate(unaryOperator(unit)) shouldBe (constant(3) o context0.root.toI)
      context0.evaluate(multiply(unaryOperator(unit), unaryOperator(unit))) shouldBe (constant(2) o context0.root.toI)

      // TODO: root contexts with a nonempty arity ... ?
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
      val carrier = makeStar(1, -1)
      val product = makeBinaryOperator(carrier,
        (1, 1) -> 1, (1, -1) -> -1, (-1, 1) -> -1, (-1, -1) -> 1
      )
      val goodMagmaWith1 = magmasWith1(carrier, unit := makeNullaryOperator(carrier, 1), multiply := product)
      goodMagmaWith1.sanityTest

      if (false) {        // TODO: fix
        val badMagmaWith1 = magmasWith1(carrier, unit := makeNullaryOperator(carrier, -1), multiply := product)
        intercept[IllegalArgumentException] {
          badMagmaWith1.sanityTest
        }.getMessage shouldBe "Left unit law failed"
      }
    }

    it("can be verified in the context of an algebraic theory, for a specified algebra") {
    }
  }
}

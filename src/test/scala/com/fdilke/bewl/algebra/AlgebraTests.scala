package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.{NativeFiniteSets, NativeFiniteSetsUtilities}
import NativeFiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import NativeFiniteSets._

class AlgebraTests extends FunSpec {
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
}

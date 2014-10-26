package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.{NativeFiniteSets, NativeFiniteSetsUtilities}
import NativeFiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import NativeFiniteSets._

class AlgebraTests extends FunSpec {
  describe("The topos algebra machinery") {
    it("can define a monoid with carrier, unit and multiplication") {
        val (i, x, y) = ('i, 'x, 'y)
        val carrier = makeStar(i, x, y)
        val unit = makeNullaryOperator(carrier, i)
        val product = makeBinaryOperator(carrier,
          (i, i) -> i, (i, x) -> x, (i, y) -> y,
          (x, i) -> x, (x, x) -> x, (x, y) -> x,
          (y, i) -> y, (y, x) -> y, (y, y) -> y
        )
        Monoid[Symbol](carrier, unit, product).sanityTest
    }

    it("checks a monoid has a left unit element") {
        val (i, x, y) = ('i, 'x, 'y)
        val carrier = makeStar(i, x, y)
        val unit = makeNullaryOperator(carrier, i)
        val product = makeBinaryOperator(carrier,
          (i, i) -> i, (i, x) -> i, (i, y) -> i,
          (x, i) -> x, (x, x) -> x, (x, y) -> x,
          (y, i) -> y, (y, x) -> y, (y, y) -> y
        )
      intercept[IllegalArgumentException] {
        Monoid[Symbol](carrier, unit, product).sanityTest
      }.
        getMessage shouldBe "Left unit law for * with unit 1"
    }

    it("checks a monoid has a right unit element") {
        val (i, x, y) = ('i, 'x, 'y)
        val carrier = makeStar(i, x, y)
        val unit = makeNullaryOperator(carrier, i)
        val product = makeBinaryOperator(carrier,
          (i, i) -> i, (i, x) -> x, (i, y) -> y,
          (x, i) -> i, (x, x) -> x, (x, y) -> y,
          (y, i) -> i, (y, x) -> x, (y, y) -> y
        )
      intercept[IllegalArgumentException] {
        Monoid[Symbol](carrier, unit, product).sanityTest
      }.
        getMessage shouldBe "Right unit law for * with unit 1"
    }

    it("checks a monoid's multiplication is associative") {
        val (i, x, y) = ('i, 'x, 'y)
        val carrier = makeStar(i, x, y)
        val unit = makeNullaryOperator(carrier, i)
        val product = makeBinaryOperator(carrier,
          (i, i) -> i, (i, x) -> x, (i, y) -> y,
          (x, i) -> x, (x, x) -> y, (x, y) -> y,
          (y, i) -> y, (y, x) -> x, (y, y) -> y
        )
      intercept[IllegalArgumentException] {
        Monoid[Symbol](carrier, unit, product).sanityTest
      }.
        getMessage shouldBe "Associative law for *"
    }

    val monoid4 = {
      val (i, x, y) = ('i, 'x, 'y)
      val carrier = makeStar(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val product = makeBinaryOperator(carrier,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      Monoid[Symbol](carrier, unit, product)
    }

    it("can define a right action for a monoid") {
      val (i, x, y) = ('i, 'x, 'y)
      val actionCarrier = makeStar('A, 'B)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      monoid4.rightAction(actionCarrier, actionMultiply).sanityTest
    }
  }
}

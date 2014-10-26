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
      val (i, x, y, a, b) = ('i, 'x, 'y, 'a, 'b)
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> a, (a, x) -> a, (a, y) -> a,
        (b, i) -> b, (b, x) -> b, (b, y) -> b
      )
      monoid4.rightAction(actionCarrier, actionMultiply).sanityTest
    }

    it("checks the right unit law for a right monoid action") {
      val (i, x, y, a, b) = ('i, 'x, 'y, 'a, 'b)
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> b, (a, x) -> a, (a, y) -> a,
        (b, i) -> a, (b, x) -> b, (b, y) -> b
      )
      intercept[IllegalArgumentException] {
        monoid4.rightAction(actionCarrier, actionMultiply).sanityTest
      }.
        getMessage shouldBe "Right unit law for * with unit 1"
    }

    it("checks the associative law for a right monoid action") {
      val (i, x, y, a, b) = ('i, 'x, 'y, 'a, 'b)
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> a, (a, x) -> b, (a, y) -> a,
        (b, i) -> b, (b, x) -> b, (b, y) -> a
      )
      intercept[IllegalArgumentException] {
        monoid4.rightAction(actionCarrier, actionMultiply).sanityTest
      }.
        getMessage shouldBe "Associative law for monoid action *"
    }
  }
}

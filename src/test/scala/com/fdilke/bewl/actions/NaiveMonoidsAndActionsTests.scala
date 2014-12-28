package com.fdilke.bewl.actions

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class NaiveMonoidsAndActionsTests extends FunSpec {

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
        NaiveMonoid[Symbol](carrier, unit, product).sanityTest
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
        NaiveMonoid[Symbol](carrier, unit, product).sanityTest
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
        NaiveMonoid[Symbol](carrier, unit, product).sanityTest
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
        NaiveMonoid[Symbol](carrier, unit, product).sanityTest
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
      NaiveMonoid[Symbol](carrier, unit, product)
    }

    it("can define a right action for a monoid") {
      val (i, x, y, a, b) = ('i, 'x, 'y, 'a, 'b)
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> a, (a, x) -> a, (a, y) -> a,
        (b, i) -> b, (b, x) -> b, (b, y) -> b
      )
      monoid4.rightAction(actionCarrier)(actionMultiply.apply).sanityTest
    }

    it("checks the right unit law for a right monoid action") {
      val (i, x, y, a, b) = ('i, 'x, 'y, 'a, 'b)
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> b, (a, x) -> a, (a, y) -> a,
        (b, i) -> a, (b, x) -> b, (b, y) -> b
      )
      intercept[IllegalArgumentException] {
        monoid4.rightAction(actionCarrier)(actionMultiply.apply).sanityTest
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
        monoid4.rightAction(actionCarrier)(actionMultiply.apply).sanityTest
      }.
        getMessage shouldBe "Associative law for monoid action *"
    }

    it("can verify if a quiver is an action morphism") {
      val monoid1x = {
        val (i, x) = ('i, 'x)
        val carrier = makeStar(i, x)
        val unit = makeNullaryOperator(carrier, i)
        val product = makeBinaryOperator(carrier,
          (i, i) -> i, (i, x) -> x,
          (x, i) -> x, (x, x) -> x
        )
        NaiveMonoid[Symbol](carrier, unit, product)
      }
      monoid1x.sanityTest
      val rightAction = {
        val (i, x, a, b) = ('i, 'x, 'a, 'b)
        val actionCarrier = makeStar(a, b)
        val actionMultiply = makeBiQuiver(actionCarrier, monoid1x.carrier, actionCarrier,
          (a, i) -> a, (a, x) -> a,
          (b, i) -> b, (b, x) -> a
        )
        monoid1x.rightAction(actionCarrier)(actionMultiply.apply)
      }  
      rightAction.sanityTest
      val rightAction2 = {
        val (i, x, c, d, e) = ('i, 'x, 'c, 'd, 'e)
        val actionCarrier = makeStar(c, d, e)
        val actionMultiply = makeBiQuiver(actionCarrier, monoid1x.carrier, actionCarrier,
          (c, i) -> c, (c, x) -> d,
          (d, i) -> d, (d, x) -> d,
          (e, i) -> e, (e, x) -> e
        )
        monoid1x.rightAction(actionCarrier)(actionMultiply.apply)
      }  
      rightAction2.sanityTest
      val actionMorphism = makeQuiver(rightAction.actionCarrier, rightAction2.actionCarrier, 
        'a -> 'd, 'b -> 'c
      )
      rightAction.isMorphism(rightAction2, actionMorphism) shouldBe true
      val nonActionMorphism = makeQuiver(rightAction.actionCarrier, rightAction2.actionCarrier, 
        'a -> 'c, 'b -> 'c
      )
      rightAction.isMorphism(rightAction2, nonActionMorphism) shouldBe false
    }
  }
}

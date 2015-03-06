package com.fdilke.bewl.actions

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class NaiveMonoidsAndActionsTests extends FunSpec {
  private val (i, x, y, a, b, c, d, e, f, f2, g, g2) =
             ('i,'x,'y,'a,'b,'c,'d,'e,'f,'f2,'g,'g2)

  describe("The topos algebra machinery") {
    it("can define a monoid with carrier, unit and multiplication") {
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
        val monoid = monoidFromTable(
          i, i, i,
          x, x, x,
          y, y, y
        )
      intercept[IllegalArgumentException] {
        monoid.sanityTest
      }.
        getMessage shouldBe "Left unit law for * with unit 1"
    }

    it("checks a monoid has a right unit element") {
        val monoid = monoidFromTable(
          i, x, y,
          i, x, y,
          i, x, y
        )
        intercept[IllegalArgumentException] {
          monoid.sanityTest
        }.
          getMessage shouldBe "Right unit law for * with unit 1"
    }

    it("checks a monoid's multiplication is associative") {
        val monoid = monoidFromTable(
          i, x, y,
          x, y, y,
          y, x, y
        )
        intercept[IllegalArgumentException] {
          monoid.sanityTest
        }.
          getMessage shouldBe "Associative law for *"
    }

    val monoid4 =
      monoidFromTable(
        i, x, y,
        x, x, x,
        y, y, y
      )

    it("can define a right action for a monoid") {
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> a, (a, x) -> a, (a, y) -> a,
        (b, i) -> b, (b, x) -> b, (b, y) -> b
      )
      monoid4.action(actionCarrier)(actionMultiply.apply).sanityTest
    }

    it("checks the right unit law for a right monoid action") {
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> b, (a, x) -> a, (a, y) -> a,
        (b, i) -> a, (b, x) -> b, (b, y) -> b
      )
      intercept[IllegalArgumentException] {
        monoid4.action(actionCarrier)(actionMultiply.apply).sanityTest
      }.
        getMessage shouldBe "Right unit law for * with unit 1"
    }

    it("checks the associative law for a right monoid action") {
      val actionCarrier = makeStar(a, b)
      val actionMultiply = makeBiQuiver(actionCarrier, monoid4.carrier, actionCarrier,
        (a, i) -> a, (a, x) -> b, (a, y) -> a,
        (b, i) -> b, (b, x) -> b, (b, y) -> a
      )
      intercept[IllegalArgumentException] {
        monoid4.action(actionCarrier)(actionMultiply.apply).sanityTest
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
        val actionCarrier = makeStar(a, b)
        val actionMultiply = makeBiQuiver(actionCarrier, monoid1x.carrier, actionCarrier,
          (a, i) -> a, (a, x) -> a,
          (b, i) -> b, (b, x) -> a
        )
        monoid1x.action(actionCarrier)(actionMultiply.apply)
      }  
      rightAction.sanityTest
      val rightAction2 = {
        val actionCarrier = makeStar(c, d, e)
        val actionMultiply = makeBiQuiver(actionCarrier, monoid1x.carrier, actionCarrier,
          (c, i) -> c, (c, x) -> d,
          (d, i) -> d, (d, x) -> d,
          (e, i) -> e, (e, x) -> e
        )
        monoid1x.action(actionCarrier)(actionMultiply.apply)
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

    it("can verify the triadic monoid") { // TODO: remove when triadic monoid is used properly for action tests
      val triadicMonoid = monoidFromTable(
          i, a, b, c, f,f2, g,g2,
          a, a, a, a, a, a, a, a,
          b, b, b, b, b, b, b, b,
          c, c, c, c, c, c, c, c,
          f, b, c, b, f2,f, b, b,
          f2,c, b, c, f,f2, c, c,
          g, c, a, a, a, a,g2, g,
          g2,a, c, c, c, c, g,g2
      )
      triadicMonoid.sanityTest
    }
  }
}

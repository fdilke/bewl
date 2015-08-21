package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicStructuresTest extends FunSpec {

  private val topos = com.fdilke.bewl.fsets.FiniteSets
  import topos._

  private val (i, x, y, a, b, c, d, e, f, f2, g, g2) =
    ('i,'x,'y,'a,'b,'c,'d,'e,'f,'f2,'g,'g2)

  describe("Monoids") {
    it("can be constructed and verified") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val product = makeBinaryOperator(carrier,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      Monoid(carrier, unit, product).sanityTest
    }

    it("enforce the left unit element") {
      intercept[IllegalArgumentException] {
        monoidFromTable(
          i, i, i,
          x, x, x,
          y, y, y
        ).sanityTest
      }.getMessage shouldBe "left unit law failed"
    }

    it("enforce the right unit law") {
      intercept[IllegalArgumentException] {
        monoidFromTable(
          i, x, y,
          i, x, y,
          i, x, y
        ).sanityTest
      }.getMessage shouldBe "right unit law failed"
    }

    it("enforce associative multiplication") {
      intercept[IllegalArgumentException] {
        monoidFromTable(
          i, x, y,
          x, y, y,
          y, x, y
        ).sanityTest
      }.getMessage shouldBe "associative law failed"
    }

    it("can validate the triadic monoid") {
      monoidFromTable(
        i, a, b, c, f,f2, g,g2,
        a, a, a, a, a, a, a, a,
        b, b, b, b, b, b, b, b,
        c, c, c, c, c, c, c, c,
        f, b, c, b, f2,f, b, b,
        f2,c, b, c, f,f2, c, c,
        g, c, a, a, a, a,g2, g,
        g2,a, c, c, c, c, g,g2
      ).sanityTest
    }

    it("can tell if a monoid is commutative") {
      monoidFromTable(
        i, a, b,
        a, a, b,
        b, b, b
      ) should be('commutative)
      monoidFromTable(
        i, a, b,
        a, a, a,
        b, b, b
      ) should not be('commutative)
    }
  }

  describe("Groups") {
    it("can be defined with an appropriate unit, multiplication and inverse") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val inverse = makeUnaryOperator(carrier,
        i -> i, x -> y, y -> x
      )
      val product = makeBinaryOperator(carrier,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> y, (x, y) -> i,
        (y, i) -> y, (y, x) -> i, (y, y) -> x
      )
      Group(carrier, unit, product, inverse).sanityTest
    }

    it("must have inverses for every element") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val inverse = makeUnaryOperator(carrier,
        i -> i, x -> y, y -> x
      )
      val product = makeBinaryOperator(carrier,
        (i, i) -> i, (i, x) -> x, (i, y) -> y,
        (x, i) -> x, (x, x) -> x, (x, y) -> x,
        (y, i) -> y, (y, x) -> y, (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        Group(carrier, unit, product, inverse).sanityTest
      }.getMessage shouldBe "left inverse law failed"
    }
  }
}

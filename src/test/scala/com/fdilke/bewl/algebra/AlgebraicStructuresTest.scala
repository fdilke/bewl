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
      new Monoid[Symbol](carrier, unit, product).sanityTest
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
  }
}

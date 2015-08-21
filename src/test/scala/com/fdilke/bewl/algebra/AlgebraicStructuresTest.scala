package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class AlgebraicStructuresTest extends FunSpec {

  private val topos = com.fdilke.bewl.fsets.FiniteSets
  import topos._
  import topos.StandardTermsAndOperators._

//  describe("Simple and compound terms") {
//    it("can describe their own free variables") {
//      α.freeVariables shouldBe Seq(α)
//      (α * β).freeVariables shouldBe Seq(α, β)
//    }
//  }

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
  }
}

package com.fdilke.bewl.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

// User: Felix Date: 30/07/2014 Time: 19:59

class BoundAlgebraicOperatorTests  extends FunSpec {
  import FiniteSets._

  describe("Algebraic operators") {
    it("can wrap arrows, be bound to sources, verify algebraic laws") {

      // 1. Construct the operators; unit and multiplication for a noncommutative monoid

      val (i, x, y) = ('i, 'x, 'y)
      val dot = set(i, x, y)
      val unit = nullaryOperator(dot, i)
      type X = Symbol
      val productOp = FiniteSetsArrow[Power[X], X](
        dot ^ 2, dot, Map(
          (i, i) -> i, (i, x) -> x, (i, y) -> y,
          (x, i) -> x, (x, x) -> x, (x, y) -> x,
          (y, i) -> y, (y, x) -> y, (y, y) -> y
        ))
      val unitOp = FiniteSetsArrow[Power[X], X](
        dot ^ 0, dot, Map(() -> i)
      )

      // 2. Bind them to a power of the carrier as source

      val source: DOT[FiniteSets.Power[Symbol]] = dot ^ 3
      val boundUnitOp = BoundAlgebraicOperator(source, unitOp)
      val boundProductOp = BoundAlgebraicOperator(source, productOp)

      // 3. Use 'universal variables' to verify algebraic laws

      var Seq(a, b, c) = (dot A 3).projection
      var u = boundUnitOp()
      boundProductOp(a, u) shouldBe a
      boundProductOp(u, a) shouldBe a
      boundProductOp(a, b) should not be (boundProductOp(b, a))
      boundProductOp(boundProductOp(a, b), c) shouldBe boundProductOp(a, boundProductOp(b, c))
    }
  }
}

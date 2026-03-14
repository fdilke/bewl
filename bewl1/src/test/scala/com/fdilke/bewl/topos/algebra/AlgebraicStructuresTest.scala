package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.{a => _, _}
import com.fdilke.bewl.helper.StandardSymbols.{
  i,
  x,
  y,
  a,
  b,
  c,
  d,
  e,
  f,
  f2,
  g,
  g2,
  r,
  s,
  commutative
}

class AlgebraicStructuresTest extends AnyFunSpec {

  import com.fdilke.bewl.fsets.FiniteSets._

  describe("Monoids") {
    it("can be constructed and verified") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val product = makeBinaryOperator(
        carrier,
        (i, i) -> i,
        (i, x) -> x,
        (i, y) -> y,
        (x, i) -> x,
        (x, x) -> x,
        (x, y) -> x,
        (y, i) -> y,
        (y, x) -> y,
        (y, y) -> y
      )
      new Monoid[Symbol](carrier, unit, product).sanityTest
    }

    it("enforce the left unit element") {
      intercept[IllegalArgumentException] {
        val carrier = dot(i, x, y)
        val unit = makeNullaryOperator(carrier, i)
        val product = makeBinaryOperator(
          carrier,
          (i, i) -> i,
          (i, x) -> i,
          (i, y) -> i,
          (x, i) -> x,
          (x, x) -> x,
          (x, y) -> x,
          (y, i) -> y,
          (y, x) -> y,
          (y, y) -> y
        )
        new Monoid[Symbol](carrier, unit, product).sanityTest
      }.getMessage shouldBe "left unit law failed"
    }

    it("enforce the right unit law") {
      intercept[IllegalArgumentException] {
        monoidFromTable(
          i,
          x,
          y,
          i,
          x,
          y,
          i,
          x,
          y
        ).sanityTest
      }.getMessage shouldBe "right unit law failed"
    }

    it("enforce associative multiplication") {
      intercept[IllegalArgumentException] {
        monoidFromTable(
          i,
          x,
          y,
          x,
          y,
          y,
          y,
          x,
          y
        ).sanityTest
      }.getMessage shouldBe "associative law failed"
    }

    it("can validate the triadic monoid") {
      monoidFromTable(
        i,
        a,
        b,
        c,
        f,
        f2,
        g,
        g2,
        a,
        a,
        a,
        a,
        a,
        a,
        a,
        a,
        b,
        b,
        b,
        b,
        b,
        b,
        b,
        b,
        c,
        c,
        c,
        c,
        c,
        c,
        c,
        c,
        f,
        b,
        c,
        b,
        f2,
        f,
        b,
        b,
        f2,
        c,
        b,
        c,
        f,
        f2,
        c,
        c,
        g,
        c,
        a,
        a,
        a,
        a,
        g2,
        g,
        g2,
        a,
        c,
        c,
        c,
        c,
        g,
        g2
      ).sanityTest
    }

    it("can tell if a monoid is commutative") {
      monoidFromTable(
        i,
        a,
        b,
        a,
        a,
        b,
        b,
        b,
        b
      ) should be(commutative)
      monoidFromTable(
        i,
        a,
        b,
        a,
        a,
        a,
        b,
        b,
        b
      ) should not be (commutative)
    }
  }

  val monoid4 =
    monoidFromTable(
      i,
      x,
      y,
      x,
      x,
      x,
      y,
      y,
      y
    )

  describe("Monoid actions") {
    it("include the off-the-shelf regular action") {
      val regularAction: monoid4.Action[Symbol] =
        monoid4.regularAction

      regularAction.sanityTest
      regularAction.actionCarrier shouldBe monoid4.carrier
    }

    it("include the off-the-shelf trivial action") {
      val sampleCarrier =
        dot("Lom", "Samazan", "Ky", "Ogar")
      val trivialAction: monoid4.Action[String] =
        monoid4.trivialAction(
          sampleCarrier
        )

      trivialAction.sanityTest
      trivialAction.actionCarrier shouldBe sampleCarrier
    }

    it("can be constructed and validated") {
      monoid4
        .action[Symbol](
          dot(a, b)
        ) {
          Function untupled Map(
            (a, i) -> a,
            (a, x) -> a,
            (a, y) -> a,
            (b, i) -> b,
            (b, x) -> b,
            (b, y) -> b
          )
        }
        .sanityTest
    }

    it("enforce the right unit law") {
      intercept[IllegalArgumentException] {
        monoid4
          .action[Symbol](
            dot(a, b)
          ) {
            Function untupled Map(
              (a, i) -> b,
              (a, x) -> a,
              (a, y) -> a,
              (b, i) -> a,
              (b, x) -> b,
              (b, y) -> b
            )
          }
          .sanityTest
      }.getMessage shouldBe "right unit law failed"
    }

    it("enforce the associative law") {
      intercept[IllegalArgumentException] {
        monoid4
          .action[Symbol](
            dot(a, b)
          ) {
            Function untupled Map(
              (a, i) -> a,
              (a, x) -> b,
              (a, y) -> a,
              (b, i) -> b,
              (b, x) -> b,
              (b, y) -> a
            )
          }
          .sanityTest
      }.getMessage shouldBe "mixed associative law failed"
    }

    it("can validate arrows as morphisms") {
      val monoid1x = {
        val carrier = dot(i, x)
        val unit = makeNullaryOperator(carrier, i)
        val product =
          makeBinaryOperator(carrier, (i, i) -> i, (i, x) -> x, (x, i) -> x, (x, x) -> x)
        new Monoid[Symbol](carrier, unit, product)
      }
      monoid1x.sanityTest
      val rightAction = {
        val actionCarrier = dot(a, b)
        val actionMultiply =
          biArrow(
            actionCarrier,
            monoid1x.carrier,
            actionCarrier
          )(
            (a, i) -> a,
            (a, x) -> a,
            (b, i) -> b,
            (b, x) -> a
          )
        new monoid1x.Action[Symbol](
          actionCarrier,
          actionMultiply
        )
      }
      rightAction.sanityTest
      val rightAction2 = {
        val actionCarrier = dot(c, d, e)
        val actionMultiply =
          biArrow(
            actionCarrier,
            monoid1x.carrier,
            actionCarrier
          )(
            (c, i) -> c,
            (c, x) -> d,
            (d, i) -> d,
            (d, x) -> d,
            (e, i) -> e,
            (e, x) -> e
          )
        new monoid1x.Action[Symbol](
          actionCarrier,
          actionMultiply
        )
      }
      rightAction2.sanityTest
      val actionMorphism = arrow(rightAction.carrier, rightAction2.carrier)(
        a -> d,
        b -> c
      )
      monoid1x.actions.isMorphism(rightAction, rightAction2, actionMorphism) shouldBe true
      val nonActionMorphism =
        arrow(
          rightAction.carrier,
          rightAction2.carrier
        )(
          a -> c,
          b -> c
        )
      monoid1x.actions.isMorphism(
        rightAction,
        rightAction2,
        nonActionMorphism
      ) shouldBe false
    }
  }

  describe("Groups") {
    it("can be defined with an appropriate unit, multiplication and inverse") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val inverse = makeUnaryOperator(carrier, i -> i, x -> y, y -> x)
      val product = makeBinaryOperator(
        carrier,
        (i, i) -> i,
        (i, x) -> x,
        (i, y) -> y,
        (x, i) -> x,
        (x, x) -> y,
        (x, y) -> i,
        (y, i) -> y,
        (y, x) -> i,
        (y, y) -> x
      )
      new Group[Symbol](
        carrier,
        unit,
        product,
        inverse
      ).sanityTest
    }

    it("must have inverses for every element") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val inverse = makeUnaryOperator(carrier, i -> i, x -> y, y -> x)
      val product = makeBinaryOperator(
        carrier,
        (i, i) -> i,
        (i, x) -> x,
        (i, y) -> y,
        (x, i) -> x,
        (x, x) -> x,
        (x, y) -> x,
        (y, i) -> y,
        (y, x) -> y,
        (y, y) -> y
      )
      intercept[IllegalArgumentException] {
        new Group[Symbol](
          carrier,
          unit,
          product,
          inverse
        ).sanityTest
      }.getMessage shouldBe "left inverse law failed"
    }

    it("can tell if a group is commutative") {
      val carrier = dot(i, x, y)
      val unit = makeNullaryOperator(carrier, i)
      val inverse = makeUnaryOperator(carrier, i -> i, x -> y, y -> x)
      val product = makeBinaryOperator(
        carrier,
        (i, i) -> i,
        (i, x) -> x,
        (i, y) -> y,
        (x, i) -> x,
        (x, x) -> y,
        (x, y) -> i,
        (y, i) -> y,
        (y, x) -> i,
        (y, y) -> x
      )
      new Group[Symbol](
        carrier,
        unit,
        product,
        inverse
      ) shouldBe commutative
    }

    it("can tell if a group is not commutative") {
      val group = groupOfUnits(
        monoidFromTable(
          i,
          a,
          b,
          c,
          r,
          s,
          a,
          i,
          s,
          r,
          c,
          b,
          b,
          r,
          i,
          s,
          a,
          c,
          c,
          s,
          r,
          i,
          b,
          a,
          r,
          b,
          c,
          a,
          s,
          i,
          s,
          c,
          a,
          b,
          i,
          r
        )
      )._1
      group.sanityTest
      group.carrier.size shouldBe 6
      group should not be commutative
    }

    it("can be regarded as monoids") {
      val largerMonoid = endomorphismMonoid(dot(1, 2, 3)).monoid
      val (group, inject) = groupOfUnits(largerMonoid)
      val monoid = group.asMonoid
      monoid.sanityTest
      monoids.isMorphism(monoid, largerMonoid, inject) shouldBe true
    }
  }

  describe("Lattices") {
    it("can be defined and verified") {
      val carrier = dot(0 to 7: _*)
      val bottom = makeNullaryOperator(carrier, 0)
      val top = makeNullaryOperator(carrier, 7)
      val meet = bifunctionAsBiArrow(carrier) { _ & _ }
      val join = bifunctionAsBiArrow(carrier) { _ | _ }

      new Lattice[Int](
        carrier,
        bottom,
        top,
        meet,
        join
      ).sanityTest
    }
  }

  describe("Heyting algebras") {
    it("can be defined and verified") {
      implicit class NotEnabledInt(n: Int) {
        val not = ~n & 7
      }

      val carrier = dot(0 to 7: _*)
      val bottom = makeNullaryOperator(carrier, 0)
      val top = makeNullaryOperator(carrier, 7)
      val meet = bifunctionAsBiArrow(carrier) {
        _ & _
      }
      val join = bifunctionAsBiArrow(carrier) {
        _ | _
      }
      val implies = bifunctionAsBiArrow(carrier) {
        _.not | _
      }

      new HeytingAlgebra[Int](
        carrier,
        bottom,
        top,
        meet,
        join,
        implies
      ).sanityTest
    }
  }
}

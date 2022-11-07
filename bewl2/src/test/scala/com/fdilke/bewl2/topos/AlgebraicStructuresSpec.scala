package com.fdilke.bewl2.topos

import com.fdilke.bewl2.algebra.AlgebraicConstructions.*
import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.Direction.*
import com.fdilke.bewl2.utility.StockStructures.*
import com.fdilke.bewl2.utility.StockSymbols.*
import com.fdilke.bewl2.utility.{Direction, RichFunSuite}
import munit.Clue.generate
import munit.FunSuite

import scala.Function.{tupled, untupled}
import scala.language.postfixOps

class AlgebraicStructuresSpec extends RichFunSuite:

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos.*
  import topos.StandardTermsAndOperators.*

  test("Algebraic theories support binary multiplication of their algebras") {
    withCyclicGroup(order = 2) {
    [Int2] => (_: Set[Int2]) ?=> (_: Int2 =:= Int) ?=> (_: Int =:= Int2) ?=> (group2: Group[Int2]) ?=>
      withCyclicGroup(order = 3) {
        [Int3] => (_: Set[Int3]) ?=> (_: Int3 =:= Int) ?=> (_: Int =:= Int3) ?=> (group3: Group[Int3]) ?=>
          withCyclicGroup(order = 6) {
            [Int6] => (_: Set[Int6]) ?=> (_: Int6 =:= Int) ?=> (_: Int =:= Int6) ?=> (group6: Group[Int6]) ?=>
              group2.sanityTest
              group3.sanityTest
              group6.sanityTest

              // TODO: bake this in, as with products of dots
              implicit val group2x3: Group[(Int2, Int3)] = group2 x group3
              group2x3.sanityTest

              groups.isMorphism(
                { _ => 0 }: ((Int2, Int3)) => Int2
              ) is true

              groups.isMorphism(
                { _ => 1 }: ((Int2, Int3)) => Int2
              ) is false

              groups.isMorphism(
                π0[Int2, Int3]
              ) is true

              groups.isMorphism(
                π1[Int2, Int3]
              ) is true

              val chineseRemainder: Int6 => (Int2, Int3) =
                { i => (i % 2, i % 3) }
              chineseRemainder.isIsoPlaceholderTrue is true

              groups.isMorphism(
                chineseRemainder
              ) is true

              val notChineseRemainder: Int6 => (Int2, Int3) =
                { i => ((i + 1) % 2, (i + 2) % 3) }
              notChineseRemainder.isIsoPlaceholderTrue is true

              groups.isMorphism(
                notChineseRemainder
              ) is false
            }
      }
    }
  }

  test("Algebraic theories support binary multiplication of their algebras, even with scalar extensions") {
    withMonoidOf3 {
      (_: Set[Symbol]) ?=> (monoidOf3: Sets.Monoid[Symbol]) ?=>
        implicit val _: Set[String] = Set("a", "b")
        val scalarMultiply: ((String, Symbol)) => String =
          case (s, m) => monoidOf3.multiply(Symbol(s), m).name

        implicit val anAction: monoidOf3.Action[String] =
          monoidOf3.action[String](scalarMultiply)
        implicit val regularAction: monoidOf3.Action[Symbol] =
          monoidOf3.regularAction

        // TODO: should be baked in
        implicit val product: monoidOf3.Action[(String, Symbol)] =
          anAction x regularAction

        product.sanityTest
        product.operatorAssignments.lookup(II).get(()) is e
        monoidOf3.actions.isMorphism[(String, Symbol), String](
          π0[String, Symbol]
        ) is true
        monoidOf3.actions.isMorphism[(String, Symbol), Symbol](
          π1[String, Symbol]
        ) is true

        val operatorsUsed =
          product.operatorAssignments.assignments map {
            _.operator
          }

        operatorsUsed.distinct.size is operatorsUsed.size
      }
    }

  test("Can construct/verify monoids") {
    implicit val _: Set[Symbol] = Set(e, a, b)
    val unit = makeNullaryOperator[Symbol](e)
    val product = makeBinaryOperator[Symbol](
      (e, e) -> e,
      (e, a) -> a,
      (e, b) -> b,
      (a, e) -> a,
      (a, a) -> a,
      (a, b) -> a,
      (b, e) -> b,
      (b, a) -> b,
      (b, b) -> b
    )
    new Monoid[Symbol](unit, product).sanityTest
  }

  test("Monoids enforce the left unit law") {
    intercept[IllegalArgumentException] {
      implicit val _: Set[Symbol] = Set(e, a, b)
      val unit = makeNullaryOperator(e)
      val product = makeBinaryOperator(
        (e, e) -> e,
        (e, a) -> e,
        (e, b) -> e,
        (a, e) -> a,
        (a, a) -> a,
        (a, b) -> a,
        (b, e) -> b,
        (b, a) -> b,
        (b, b) -> b
      )
      new Monoid[Symbol](unit, product).sanityTest
    }.getMessage is "left unit law failed"
  }

  test("Monoids enforce the right unit law") {
    intercept[IllegalArgumentException] {
      withMonoidFromTable(
        e, a, b,
        e, a, b,
        e, a, b
      ) {
        (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
        monoid.sanityTest
      }
    }.getMessage is "right unit law failed"
  }

  test("Monoids enforce associative multiplication") {
    intercept[IllegalArgumentException] {
      withMonoidFromTable(
        e, a, b,
        a, b, b,
        b, a, b
      ) {
        (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
          monoid.sanityTest
      }
    }.getMessage is "associative law failed"
  }

  test("Monoids can validate the triadic monoid") {
    val Seq(i, a, b, c, f, f2, g, g2) =
      Seq[String]("i", "a", "b", "c", "f", "f2", "g", "g2").map { Symbol(_) }
    withMonoidFromTable(
      i, a, b, c, f,f2, g,g2,
      a, a, a, a, a, a, a, a,
      b, b, b, b, b, b, b, b,
      c, c, c, c, c, c, c, c,
      f, b, c, b,f2, f, b, b,
      f2,c, b, c, f,f2, c, c,
      g, c, a, a, a, a,g2,g,
      g2,a, c, c, c, c, g,g2
    ) {
      (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
        monoid.sanityTest
    }
  }

  test("Monoids can test commutativity") {
    withMonoidFromTable(
      e, a, b,
      a, a, b,
      b, b, b
    ) {
        (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
          monoid.isCommutative is true
      }
    withMonoidFromTable(
      e, a, b,
      a, a, a,
      b, b, b
    ) {
      (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
        monoid.isCommutative is false
    }
  }

  test("Monoids include the off-the-shelf regular action") {
    withMonoidOf3a {
      (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
        val regularAction: monoid.Action[Symbol] =
          monoid.regularAction

        regularAction.sanityTest
    }
  }

  test("Monoid actions include the off-the-shelf trivial action") {
    implicit val _: Set[String] =
      Set("Lom", "Samazan", "Ky", "Ogar")
    withMonoidOf3a {
      (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
      val trivialAction: monoid.Action[String] =
        monoid.trivialAction[String]

      trivialAction.sanityTest
    }
  }

  test("Monoid actions can be constructed and validated") {
    maskSetDot(
      Set[Symbol](a, b)
    ) {
      [S] => (_: Set[S]) ?=> (_: S =:= Symbol) ?=> (_: Symbol =:= S) ?=>
      withMonoidOf3a {
        (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
          monoid.action[S](
            Map[(S, Symbol), S](
              (a : S, e) -> (a : S),
              (a : S, a) -> (a : S),
              (a : S, b) -> (a : S),
              (b : S, e) -> (b : S),
              (b : S, a) -> (b : S),
              (b : S, b) -> (b : S)
            )
          ).sanityTest
        }
      }
  }

  test("Monoid actions can be constructed and validated (simpler version)") {
    implicit val _: Set[Int] = Set(1, 2)
    withMonoidOf3a {
      (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
        monoid.action[Int](
          Map(
            (1, e) -> 1,
            (1, a) -> 1,
            (1, b) -> 1,
            (2, e) -> 2,
            (2, a) -> 2,
            (2, b) -> 2
          )
        ).sanityTest
      }
  }

  test("Monoid actions enforce the right unit law") {
    implicit val _: Set[Int] = Set(1, 2)
    intercept[IllegalArgumentException] {
      withMonoidOf3a {
        (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
          monoid.action[Int](
          Map(
          (1, e) -> 2,
          (1, a) -> 1,
          (1, b) -> 1,
          (2, e) -> 1,
          (2, a) -> 2,
          (2, b) -> 2
        )
        ).sanityTest
      }
    }.getMessage is "right unit law failed"
  }

  test("Monoid actions enforce the associative law") {
    implicit val _: Set[Int] = Set(1, 2)
    intercept[IllegalArgumentException] {
      withMonoidOf3a {
        (_: Set[Symbol]) ?=> (monoid: Sets.Monoid[Symbol]) ?=>
          monoid.action[Int](
            Map(
              (1, e) -> 1,
              (1, a) -> 2,
              (1, b) -> 1,
              (2, e) -> 2,
              (2, a) -> 2,
              (2, b) -> 1
            )
          ).sanityTest
      }
    }.getMessage is "mixed associative law failed"
  }

  test("Monoid actions can validate arrows as morphisms") {
    withMonoid_1_0 {
      (_: Set[Int]) ?=> (monoid_1_0: Sets.Monoid[Int]) ?=>
      monoid_1_0.sanityTest
      implicit val _: Set[Symbol] = Set(a, b)
      implicit val rightAction: monoid_1_0.Action[Symbol] =
        monoid_1_0.action[Symbol](
          Map(
            (a, 1) -> a,
            (a, 0) -> a,
            (b, 1) -> b,
            (b, 0) -> a
          )
        )
      rightAction.sanityTest
      implicit val _:Set[String] = Set("c", "d", "e")
      implicit val rightAction2: monoid_1_0.Action[String] =
        monoid_1_0.action[String](
          Map(
            ("c", 1) -> "c",
            ("c", 0) -> "d",
            ("d", 1) -> "d",
            ("d", 0) -> "d",
            ("e", 1) -> "e",
            ("e", 0) -> "e"
          )
        )
      rightAction2.sanityTest
      val actionMorphism: Symbol => String = Map(
        a -> "d",
        b -> "c"
      )
      monoid_1_0.actions.isMorphism(
        actionMorphism
      ) is true
      val nonActionMorphism: Symbol => String = Map(
        a -> "c",
        b -> "c"
      )
      monoid_1_0.actions.isMorphism(
        nonActionMorphism
      ) is false
   }
  }

  test("Groups can be defined with an appropriate unit, multiplication and inverse") {
    implicit val _: Set[Symbol] = Set(e, a, b)
    val unit = makeNullaryOperator(e)
    val inverse = makeUnaryOperator(e -> e, a -> b, b -> a)
    val product = makeBinaryOperator(
      (e, e) -> e,
      (e, a) -> a,
      (e, b) -> b,
      (a, e) -> a,
      (a, a) -> b,
      (a, b) -> e,
      (b, e) -> b,
      (b, a) -> e,
      (b, b) -> a
    )
    new Sets.Group[Symbol](
      unit,
      product,
      inverse
    ).sanityTest
  }

  test("Groups must have inverses for every element") {
    implicit val _: Set[Symbol] = Set(e, a, b)
    val unit = makeNullaryOperator(e)
    val inverse = makeUnaryOperator(e -> e, a -> b, b -> a)
    val product = makeBinaryOperator(
      (e, e) -> e,
      (e, a) -> a,
      (e, b) -> b,
      (a, e) -> a,
      (a, a) -> a,
      (a, b) -> a,
      (b, e) -> b,
      (b, a) -> b,
      (b, b) -> b
    )
    intercept[IllegalArgumentException] {
      new Sets.Group[Symbol](
        unit,
        product,
        inverse
      ).sanityTest
    }.getMessage is "left inverse law failed"
  }

  test("Groups can tell if a group is commutative or not") {
    withSymmetricGroup(2) {
      (_: Set[Seq[Int]]) ?=> (group: Group[Seq[Int]]) ?=>
        group.isCommutative is true
    }
    withSymmetricGroup(3) {
      (_: Set[Seq[Int]]) ?=> (group: Group[Seq[Int]]) ?=>
        group.isCommutative is false
    }
  }

  test("Groups verify explicitly that S_3 is not commutative") {
    with_S_3 {
      (carrier: Set[Symbol]) ?=> (group: Group[Symbol]) ?=>
        carrier.size is 6
        group.sanityTest
        group.isCommutative is false
    }
  }

  test("Groups can be regarded as monoids") {
    implicit val _: Set[Int] = Set(1, 2, 3)
    withEndomorphismMonoid[Int, Unit] {
      [E] => (_: Set[E]) ?=> (largerMonoid: EndomorphismMonoid[E, Int]) ?=>
        withGroupOfUnits[E, Unit] {
          [U] => (_: Set[U]) ?=> (groupU: Group[U]) ?=> (embed: U => E) =>
            implicit val monoidU: Monoid[U] = groupU.asMonoid
            monoidU.sanityTest
            monoids.isMorphism(embed) is true
          }
    }
  }

  test("Lattices can be defined and verified") {
    implicit val _: Set[Int] = 0 to 7 toSet
    val bottom: Unit => Int = makeNullaryOperator[Int](0)
    val top: Unit => Int = makeNullaryOperator[Int](7)
    val meet: ((Int, Int)) => Int = tupled { _ & _ }
    val join: ((Int, Int)) => Int = tupled { _ | _ }

    new Lattice[Int](
      bottom,
      top,
      meet,
      join
    ).sanityTest
  }

/*

  }

  describe("Lattices") {
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
*/

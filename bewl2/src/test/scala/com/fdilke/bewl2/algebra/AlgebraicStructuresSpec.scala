package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.algebra.AlgebraicConstructions.*
import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.StockStructures.*
import com.fdilke.bewl2.utility.StockSymbols.*
import com.fdilke.bewl2.utility.RichFunSuite
import munit.Clue.generate
import munit.FunSuite

import scala.Function.{tupled, untupled}
import scala.language.postfixOps
import com.fdilke.bewl2.sets.Sets
import Sets.*
import Sets.StandardTermsAndOperators.*
import com.fdilke.utility.Shortcuts.*

class AlgebraicStructuresSpec extends RichFunSuite:

  test("Algebraic theories support binary multiplication of their algebras") {
    withCyclicGroup(order = 2) {
    [Int2] => (_: Dot[Int2]) ?=> (_: Int2 =:= Int) ?=> (_: Int =:= Int2) ?=> (group2: Group[Int2]) ?=>
      withCyclicGroup(order = 3) {
        [Int3] => (_: Dot[Int3]) ?=> (_: Int3 =:= Int) ?=> (_: Int =:= Int3) ?=> (group3: Group[Int3]) ?=>
          withCyclicGroup(order = 6) {
            [Int6] => (_: Dot[Int6]) ?=> (_: Int6 =:= Int) ?=> (_: Int =:= Int6) ?=> (group6: Group[Int6]) ?=>
              group2.sanityTest
              group3.sanityTest
              group6.sanityTest

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
              chineseRemainder.isIso is true

              groups.isMorphism(
                chineseRemainder
              ) is true

              val notChineseRemainder: Int6 => (Int2, Int3) =
                { i => ((i + 1) % 2, (i + 2) % 3) }
              notChineseRemainder.isIso is true

              groups.isMorphism(
                notChineseRemainder
              ) is false
            }
      }
    }
  }

  test("Algebraic theories support binary multiplication of their algebras, even with scalar extensions") {
    withMonoidOf3 {
      (_: Dot[Symbol]) ?=> (monoidOf3: Monoid[Symbol]) ?=>
        withDot(Set[String]("a", "b")) {
          val scalarMultiply: ((String, Symbol)) => String =
            case (s, m) => monoidOf3.multiply(Symbol(s), m).name

          monoidOf3.withAction(scalarMultiply) {
            monoidOf3.withRegularAction {

              implicit val product: monoidOf3.Action[(String, Symbol)] =
                summon[monoidOf3.Action[String]] x summon[monoidOf3.Action[Symbol]]

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
        }
      }
    }

  test("Can construct/verify monoids") {
    withDot(Set[Symbol](e, a, b)) {
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
  }

  test("Monoids enforce the left unit law") {
    withDot(Set[Symbol](e, a, b)) {
      intercept[IllegalArgumentException] {
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
  }

  test("Monoids enforce the right unit law") {
    intercept[IllegalArgumentException] {
      withMonoidFromTable(
        e, a, b,
        e, a, b,
        e, a, b
      ) {
        (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
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
        (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
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
      (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
        monoid.sanityTest
    }
  }

  test("Monoids can test commutativity") {
    withMonoidFromTable(
      e, a, b,
      a, a, b,
      b, b, b
    ) {
        (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
          monoid.isCommutative is true
      }
    withMonoidFromTable(
      e, a, b,
      a, a, a,
      b, b, b
    ) {
      (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
        monoid.isCommutative is false
    }
  }

  test("Monoids include the off-the-shelf regular action") {
    withMonoidOf3a {
      val monoid: Monoid[Symbol] = summon
      monoid.withRegularAction {
        summon[monoid.Action[Symbol]].sanityTest
      }
    }
  }

  test("Monoid actions include the off-the-shelf trivial action") {
    withDot(Set[String]("Lom", "Samazan", "Ky", "Ogar")) {
      withMonoidOf3a {
        val monoid: Monoid[Symbol] = summon
        monoid.withTrivialAction[String, Unit] {
          summon[monoid.Action[String]].sanityTest
        }
      }
    }
  }

  test("Monoid actions can be constructed and validated") {
    withDotMask(
      Set[Symbol](a, b)
    ) {
      [S] => (_: Dot[S]) ?=> (_: S =:= Symbol) ?=> (_: Symbol =:= S) ?=>
      withMonoidOf3a {
        (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
          monoid.withAction[S, Unit](
            Map[(S, Symbol), S](
              (a : S, e) -> (a : S),
              (a : S, a) -> (a : S),
              (a : S, b) -> (a : S),
              (b : S, e) -> (b : S),
              (b : S, a) -> (b : S),
              (b : S, b) -> (b : S)
            )
          ){
            summon[monoid.Action[S]].sanityTest
          }
        }
      }
  }

  test("Monoid actions can be constructed and validated (simpler version)") {
    withDot(Set[Int](1, 2)) {
      withMonoidOf3a { (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
        monoid.withAction[Int, Unit](
          Map(
            (1, e) -> 1,
            (1, a) -> 1,
            (1, b) -> 1,
            (2, e) -> 2,
            (2, a) -> 2,
            (2, b) -> 2
          )
        ) {
          summon[monoid.Action[Int]].sanityTest
        }
      }
    }
  }

  test("Monoid actions enforce the right unit law") {
    withDot(Set[Int](1, 2)) {
      intercept[IllegalArgumentException] {
        withMonoidOf3a {
          (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
            monoid.withAction[Int, Unit](
              Map(
              (1, e) -> 2,
              (1, a) -> 1,
              (1, b) -> 1,
              (2, e) -> 1,
              (2, a) -> 2,
              (2, b) -> 2
            )
          ) {
              summon[monoid.Action[Int]].sanityTest
            }
        }
      }.getMessage is "right unit law failed"
    }
  }

  test("Monoid actions enforce the associative law") {
    withDot(Set[Int](1, 2)) {
      intercept[IllegalArgumentException] {
        withMonoidOf3a {
          (_: Dot[Symbol]) ?=> (monoid: Monoid[Symbol]) ?=>
            monoid.withAction[Int, Unit](
              Map(
                (1, e) -> 1,
                (1, a) -> 2,
                (1, b) -> 1,
                (2, e) -> 2,
                (2, a) -> 2,
                (2, b) -> 1
              )
            ) {
              summon[monoid.Action[Int]].sanityTest
            }
        }
      }.getMessage is "mixed associative law failed"
    }
  }

  test("Monoid actions can validate arrows as morphisms") {
    withMonoid_1_0 { (_: Dot[Int]) ?=> (monoid_1_0: Monoid[Int]) ?=>
      monoid_1_0.sanityTest
      withDot(Set[Symbol](a, b)) {
        monoid_1_0.withAction[Symbol, Unit](
            Map(
              (a, 1) -> a,
              (a, 0) -> a,
              (b, 1) -> b,
              (b, 0) -> a
            )
          ) {
          summon[monoid_1_0.Action[Symbol]].sanityTest
          withDot(Set[String]("c", "d", "e")) {
            monoid_1_0.withAction[String, Unit](
              Map(
                ("c", 1) -> "c",
                ("c", 0) -> "d",
                ("d", 1) -> "d",
                ("d", 0) -> "d",
                ("e", 1) -> "e",
                ("e", 0) -> "e"
              )
            ) {
              summon[monoid_1_0.Action[String]].sanityTest
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
        }
      }
    }
  }

  test("Groups can be defined with an appropriate unit, multiplication and inverse") {
    withDot(Set[Symbol](e, a, b)) {
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
      new Group[Symbol](
        unit,
        product,
        inverse
      ).sanityTest
    }
  }

  test("Groups must have inverses for every element") {
    withDot(Set[Symbol](e, a, b)) {
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
        new Group[Symbol](
          unit,
          product,
          inverse
        ).sanityTest
      }.getMessage is "left inverse law failed"
    }
  }

  test("Groups can tell if a group is commutative or not") {
    withSymmetricGroup(2) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      group.isCommutative is true
    }
    withSymmetricGroup(3) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      group.isCommutative is false
    }
  }

  test("Groups verify explicitly that S_3 is not commutative") {
    with_S_3 {
      (_: Dot[Symbol]) ?=> (group: Group[Symbol]) ?=>
        dot[Symbol].size is 6
        group.sanityTest
        group.isCommutative is false
    }
  }

  test("Groups can be regarded as monoids") {
    withDot(Set[Int](1, 2, 3)) {
      withEndomorphismMonoid[Int, Unit] {
        [E] => (_: Dot[E]) ?=> (largerMonoid: Monoid[E]) ?=> (standardAction: largerMonoid.Action[Int]) ?=>
          withGroupOfUnits[E, Unit] {
            [U] => (_: Dot[U]) ?=> (groupU: Group[U]) ?=> (embed: U => E) =>
              groupU.withMonoid {
                summon[Monoid[U]].sanityTest
                monoids.isMorphism[U, E](embed) is true
              }
            }
      }
    }
  }

  test("A group action can be induced from a monoid one via a morphism") {
    withCyclicGroup(4) { [I] => (_: Dot[I]) ?=> (i2int : I =:= Int) ?=> (_ : Int =:= I) ?=> (groupI : Group[I]) ?=>
      withCyclicGroup(4) { [J] => (_: Dot[J]) ?=>(_: J =:= Int) ?=> (int2j: Int =:= J) ?=> (groupJ: Group[J]) ?=>
        val iToJ: I => J = { x =>
          transmute[I, Int, J](groupI.multiply(x, x))
        }
        groups.isMorphism(iToJ) is true
        groupJ.withMonoid{ (monoidJ: Monoid[J]) ?=>
          monoidJ.withRegularAction { (actionJ: monoidJ.Action[J]) ?=>
            val actionI: groupI.Action[J] = actionJ.induced(iToJ)
            actionI.sanityTest
            actionI.actionMultiply(1, 3) is 3
          }
        }
      }
    }
  }

  test("Pick out the subgroup preserving an arrow from an action") {
    withDots(
      Set[String]("a", "b"),
      Set[Int](0, 1)
    ) {
      withAutomorphismGroup[(String, Int), Unit] { 
        [A] => (_ : Dot[A]) ?=> (group: Group[A]) ?=> (action: group.Action[(String, Int)]) ?=>
          // val arrow: ((String, Int) => Int = π0[String, Int]
          action.preserving(
            π0[String, Int]
          ) { [P] => (_ : Dot[P]) ?=> (groupP: Group[P]) ?=> (embed: P => A) =>
            groupP.sanityTest
            dot[P].size is 4
            // dot[P].map(embed) is Set[Seq[Int]]( Seq(0, 1, 2), Seq(1, 2, 0), Seq(2, 0, 1) )
            action.induced[P](embed).sanityTest
          }
      }
    }
  }

  test("The action machinery for monoids also works with groups") {
    withCyclicGroup(order = 6) { [Int6] => (_: Dot[Int6]) ?=> (_: Int6 =:= Int) ?=> (_: Int =:= Int6) ?=> (group6: Group[Int6]) ?=>
      withDot(Set[Int](0, 1)) {
        group6.withAction[Int, Unit] {
          case (a: Int, m: Int6) => (a + m) % 2
        } {
          summon[group6.Action[Int]].sanityTest
          group6.withRegularAction {
            group6.actions.isMorphism[Int6, Int] {
              _ % 2
            } is true
          }
        }
      }
    }
  }

  test("Lattices can be defined and verified") {
    withDot(0 to 7 toSet : Set[Int]) {
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
  }

  test("Heyting algebras can be defined and verified") {
      extension(n: Int)
        def not = ~n & 7

      withDot(0 to 7 toSet : Set[Int]) {
        val bottom: Unit => Int = makeNullaryOperator(0)
        val top: Unit => Int = makeNullaryOperator(7)
        val meet: ((Int, Int)) => Int = tupled { _ & _ }
        val join: ((Int, Int)) => Int = tupled { _ | _ }
        val implies: ((Int, Int)) => Int = tupled { _.not | _ }

        new HeytingAlgebra[Int](
          bottom,
          top,
          meet,
          join,
          implies
        ).sanityTest
      }
    }

  test("Can use infix * for multiplication in the context of a group") {
    withMonoidOf3 { (_: Dot[Symbol]) ?=> (monoidOf3: Monoid[Symbol]) ?=>
      (a * e) is a
      (a * b) is b
    }
  }

  test("Can use infix * for multiplication and unary ~ for inversion in the context of a group") {
    withSymmetricGroup(3) { (ints : Sets.Dot[Int], seqs: Sets.Dot[Seq[Int]], group: Sets.Group[Seq[Int]]) ?=> (action: group.Action[Int]) => 
      val a: Seq[Int] = Seq(1, 0, 2)
      val b: Seq[Int] = Seq(0, 2, 1)
      (a * b) is Seq(2, 0, 1)
      ~(a * b) is Seq(1, 2, 0)
    }
  }
  
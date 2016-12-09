package com.fdilke.bewl.topos

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._
import com.fdilke.bewl.helper.{Memoize, ⊕}

import scala.Function.untupled

class DotAndArrowEnrichmentTest extends FunSpec {

  describe("The universal quantifier") {
    it("detects whether a subobject is the whole object") {
      val totalSet = dot(1, 2, 3, 4)
      val subset = dot(1, 3)

      val embed = subset(totalSet) { x => x }
      val ∀ = totalSet.∀
      ∀ should have(
        'source(totalSet > omega),
        'target(omega)
      )
      ∀ o embed.chi.name should not be truth
      ∀ o totalSet.identity.chi.name shouldBe truth
    }
  }

  describe("The existential quantifier") {
    it("detects whether a subobject is NOT empty") {
      val totalSet = dot(1, 2, 3, 4)
      val subset = dot(1, 3)
      val emptySet = dot[Int]()

      val embed = subset(totalSet) { x => x}
      val embedEmpty = emptySet(totalSet) { x => x}
      val exists = totalSet.∃
      exists should have(
        'source(totalSet > omega),
        'target(omega)
      )
      exists o embed.chi.name shouldBe truth
      exists o embedEmpty.chi.name should not be truth
      exists o totalSet.identity.chi.name shouldBe truth
    }
  }

  describe("Sequence comprehensions for operators") {
    it("can define unary operators") {
      val set = dot(-1, 0, 1)
      val unaryMinus =
        for (x <- set)
          yield -x

      unaryMinus shouldBe arrow(set, set)(
        -1 -> 1, 0 -> 0, 1 -> -1
      )

      unaryMinus shouldBe {
        set map { -_ }
      }
    }

    it("can define binary operators") {
      val three = dot(0, 1, 2)
      val subtract = for (x <- three ; y <- three) yield (x - y + 3) % 3;
      subtract shouldBe makeBinaryOperator(three,
        (0, 0) -> 0, (0, 1) -> 2, (0, 2) -> 1,
        (1, 0) -> 1, (1, 1) -> 0, (1, 2) -> 2,
        (2, 0) -> 2, (2, 1) -> 1, (2, 2) -> 0
      )
    }
  }

  describe("The truth object") {
    def theBinOp(binop: (Boolean, Boolean) => Boolean): BinaryOp[TRUTH] =
      for(i <- omega ; j <- omega)
        yield binop(i, j)

    it("has the correct binary operations for binary operations") {
      Ω.meet shouldBe theBinOp { _ & _ }
      Ω.join shouldBe theBinOp { _ | _ }
      Ω.implies shouldBe theBinOp { !_ | _ }
      falsity shouldBe I(omega) {
        _ => false
      }
    }
  }

  describe("The equality comparison arrow") {
    it("should have the correct value for finite sets") {
      val set = dot(0, 1)
      set.=?= shouldBe biArrow(set, set, omega,
        (0,0) -> true, (0, 1) -> false,
        (1,0) -> false,(1,1) -> true
      )
    }
  }

  describe("Functional relations") {
    it("can be converted to arrows") {
      val symbols = dot('A, 'B, 'C)
      val numbers = dot(1, 2, 3)
      symbols.arrowFromFunctionalRelation(numbers)(untupled(Map(
        ('A, 1) -> false, ('A, 2) -> true,  ('A, 3) -> false,
        ('B, 1) -> true,  ('B, 2) -> false, ('B, 3) -> false,
        ('C, 1) -> true,  ('C, 2) -> false, ('C, 3) -> false
      ))) shouldBe
        arrow(symbols, numbers)(
          'A -> 2, 'B -> 1, 'C -> 1
        )
    }
  }

  describe("The partial arrow classifier") {
    it("should have the correct attributes for finite sets") {
      val set = dot(0, 1)
      val pac = set.pac
      pac.classifier.globals should have size 3
      pac.include should have (
        'source(set),
        'target(pac.classifier)
      )
      pac.include shouldBe 'monic
      pac.⏊ should have (
        'source(I),
        'target(pac.classifier)
      )
      pac.⏊ shouldBe pac.extend(O.toI, set.fromO)
      Seq(0, 1) map pac.⏊(()) shouldBe Seq(false, false)

      val foo = dot('a, 'b)
      val subFoo = dot(true)
      val inclusion = arrow(subFoo, foo)(true -> 'a)
      val subFoo2set = arrow(subFoo, set)(true -> 1)

      val foo2setStar = pac.extend(inclusion, subFoo2set)
      foo2setStar should have (
        'source(foo),
        'target(pac.classifier)
      )
      val imageOf0 = pac.include(0)
      val imageOf1 = pac.include(1)
      foo2setStar('a) shouldBe imageOf1

      foo2setStar('b) shouldBe pac.⏊(())
      foo2setStar('b) should not be imageOf0
      foo2setStar('b) should not be imageOf1
    }
  }

  describe("Coproducts") {
    it("should give the expected construction for sets") {
      val foo = dot(0, 1)
      val bar = dot('a, 'b, 'c)
      val coproduct = foo + bar

      coproduct.globals should have size 5
      foo +- bar should have (
        'source(foo),
        'target(coproduct),
        'monic(true)
      )
      foo -+ bar should have (
        'source(bar),
        'target(coproduct),
        'monic(true)
      )

      val target = dot("P", "Q", "R")
      val foo2target = arrow(foo, target)(0 -> "Q", 1 -> "P")
      val bar2target = arrow(bar, target)('a -> "R", 'b -> "P", 'c -> "Q")
      val sum = foo2target + bar2target

      sum should have (
        'source(coproduct),
        'target(target)
      )
      foo2target shouldBe (sum o (foo +- bar))
      bar2target shouldBe (sum o (foo -+ bar))
    }
  }

  describe("The projection operations *-, -*") {
    it("are correctly calculated for sets") {
      val symbols = dot('A, 'B, 'C)
      val numbers = dot(1, 2, 3)

      symbols *- numbers shouldBe
        (symbols x numbers) (symbols) {
          case s ⊕ _ => s
        }

      symbols -* numbers shouldBe
        (symbols x numbers) (numbers) {
          case _ ⊕ n => n
        }
    }
  }

  describe("The contravariant exponential functor H ^ _") {
    it("behaves as expected") {
      val h = dot(true, false)
      val symbols = dot('A, 'B, 'C)
      val numbers = dot(1, 2, 3)
      val f: Symbol > Int = arrow(symbols, numbers)(
        'A -> 2, 'B -> 1, 'C -> 1
      )
      val h_f: (Int → Boolean) > (Symbol → Boolean) = h > f

      for (
        g <- elementsOf(numbers > h) ;
        symbol <- Seq('A, 'B, 'C)
      )
        h_f(g)(symbol) shouldBe g(f(symbol))
    }
  }
}

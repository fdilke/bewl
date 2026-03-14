package com.fdilke.bewl2.topos.enrichment

import org.scalatest.funspec.AnyFunSpec
import com.fdilke.bewl2.fsets.FiniteSets.FiniteSetsTopos._
import com.fdilke.bewl2.util.FunctionWithEquality
import org.scalatest.matchers.should.Matchers._

import scala.Function.untupled

class EnrichmentTests extends AnyFunSpec {

  describe("Function comparisons") {
    it("work for unary functions") {
      val theId: Boolean => Boolean = id[Boolean]
      val constT: Boolean => Boolean = { _ => true }
      (theId =?= theId) shouldBe true
      (theId =?= id[Boolean]) shouldBe true
      (theId =?= constT) shouldBe false

      theId shouldBeFn theId
      theId shouldBeFn id[Boolean]
      theId shouldNotBeFn constT

      intercept[IllegalArgumentException](theId shouldNotBeFn theId)
      intercept[IllegalArgumentException](theId shouldNotBeFn id[Boolean])
      intercept[IllegalArgumentException](theId shouldBeFn constT)
    }

    it("work for binary functions") {
      val pi0: (Boolean, Boolean) => Boolean = { (x, _) => x }
      val and: (Boolean, Boolean) => Boolean = { _ & _ }
      val otherAnd: (Boolean, Boolean) => Boolean = { (x, y) => if (x) y else false }
      val xor: (Boolean, Boolean) => Boolean = { _ ^ _ }

      (pi0 =?= pi0) shouldBe true
      (pi0 =?= untupled(π0[Boolean, Boolean])) shouldBe true
      (pi0 =?= xor) shouldBe false
      (and =?= xor) shouldBe false
      (and =?= otherAnd) shouldBe true

      pi0 shouldBeFn pi0
      pi0 shouldBeFn untupled(π0[Boolean, Boolean])
      pi0 shouldNotBeFn xor
      and shouldNotBeFn xor
      and shouldBeFn otherAnd

      intercept[IllegalArgumentException](pi0 shouldNotBeFn pi0)
      intercept[IllegalArgumentException](pi0 shouldNotBeFn untupled(π0[Boolean, Boolean]))
      intercept[IllegalArgumentException](pi0 shouldBeFn xor)
      intercept[IllegalArgumentException](and shouldBeFn xor)
      intercept[IllegalArgumentException](and shouldNotBeFn otherAnd)
    }
  }

  describe("The universal quantifier") {
    it("detects whether a subobject is the whole object") {
      implicit val whole: Iterable[Int] = Iterable(1, 2, 3, 4)
      implicit val part: Iterable[Seq[Int]] = Iterable(Seq(1), Seq(3))

      val embed: Seq[Int] => Int = _.head
      val forall: (Int > Ω) => Ω = ∀[Int]
      forall.source shouldBe dot[Int > Ω]
      forall.target shouldBe dot[Ω]

      (forall.o(embed.chi.chi.name)) shouldNotBeFn truth
      (forall.o(id[Int].chi.chi.name)) shouldBeFn truth
    }
  }

  describe("The truth object") {
    it("has the correct binary operations for logical operations") {
      import LogicalOperations._

      and shouldBeFn { _ & _ }
      implies shouldBeFn { !_ | _ }
      or shouldBeFn { _ | _ }
      falsity shouldBeFn { _ => false }
    }

    it("has the correct binary operations for logical operations as enrichments") {
      { (_: Ω) ∧ (_: Ω) } shouldBeFn { _ & _ }
      { (_: Ω) → (_: Ω) } shouldBeFn { !_ | _ }
      { (_: Ω) ∨ (_: Ω) } shouldBeFn { _ | _ }
    }
  }

  describe("The existential quantifier") {
    it("detects whether a subobject is NOT empty") {
      implicit val totalSet: Iterable[Int] = Iterable(1, 2, 3, 4)
      implicit val subset: Iterable[Option[Int]] = Iterable(Some(1), Some(3))
      implicit val emptySet: Iterable[String] = Iterable()

      val embed: Option[Int] => Int = { _.get }
      val embedEmpty: String => Int = { _.length }
      val exists: (Int > Boolean) => Boolean = ∃[Int]
      exists.source shouldBe dot[Int > Boolean]
      exists.target shouldBe dot[Boolean]

      (exists.o(embed.chi.chi.name)) shouldBeFn truth
      (exists.o(embedEmpty.chi.chi.name)) shouldNotBeFn truth
      (exists.o(id[Int].chi.chi.name)) shouldBeFn truth
    }
  }
  /*
    it("can be used over a dot to make an arrow into omega") {
      val carrier = dot(1,2,3)
      val containers = dot(Set(1,2), Set(2))

      carrier.exists(containers) {
        (s, c) =>
          c contains s
      } shouldBe {
        carrier(omega) {
          _ < 3
        }
      }
    }

    it("can be used inside a biproduct") {
      val left = dot(true, false)
      val mid = dot("Johnny", "Wulf", "Gronk")
      val right = dot(1,2,3,4)

      val product = left x right

      product.existsMid(mid) {
        (l, m, r) =>
          l && (m == "Wulf") && (r > 2)
      } shouldBe {
        product.biArrow(omega) {
          (l, r) =>
            l && (r > 2)
        }
      }
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
      val subtract =
        for {
          x <- three
          y <- three
        }
          yield (x - y + 3) % 3
      subtract shouldBe makeBinaryOperator(three,
        (0, 0) -> 0, (0, 1) -> 2, (0, 2) -> 1,
        (1, 0) -> 1, (1, 1) -> 0, (1, 2) -> 2,
        (2, 0) -> 2, (2, 1) -> 1, (2, 2) -> 0
      )
    }
  }


  describe("The equality comparison arrow") {
    it("should have the correct value for finite sets") {
      val set = dot(0, 1)
      set.=?= shouldBe biArrow(
        set,
        set,
        omega
      )(
        (0,0) -> true,
        (0, 1) -> false,
        (1,0) -> false,
        (1,1) -> true
      )
    }
  }

  describe("Functional relations") {
    it("can be converted to arrows") {
      val symbols = dot(a, b, c)
      val numbers = dot(1, 2, 3)
      symbols.arrowFromFunctionalRelation(
        numbers
      )(
        untupled(
          Map(
            (a, 1) -> false, (a, 2) -> true,  (a, 3) -> false,
            (b, 1) -> true,  (b, 2) -> false, (b, 3) -> false,
            (c, 1) -> true,  (c, 2) -> false, (c, 3) -> false
          )
        )
      ) shouldBe
        arrow(symbols, numbers)(
          a -> 2, b -> 1, c -> 1
        )
    }
  }

  describe("The partial arrow classifier") {
    it("should have the correct attributes for finite sets") {
      val set = dot(0, 1)
      val pac = set.pac
      pac.classifier should have size 3
      pac.include should have (
        source(set),
        target(pac.classifier)
      )
      pac.include shouldBe monic
      pac.⏊ should have (
        source(I),
        target(pac.classifier)
      )
      pac.⏊ shouldBe pac.extend(O.toI, set.fromO)
      Seq(0, 1) map pac.⏊(()) shouldBe Seq(false, false)

      val foo = dot(a, b)
      val subFoo = dot(true)
      val inclusion = arrow(subFoo, foo)(true -> a)
      val subFoo2set = arrow(subFoo, set)(true -> 1)

      val foo2setStar = pac.extend(inclusion, subFoo2set)
      foo2setStar should have (
        source(foo),
        target(pac.classifier)
      )
      val imageOf0 = pac.include(0)
      val imageOf1 = pac.include(1)
      foo2setStar(a) shouldBe imageOf1

      foo2setStar(b) shouldBe pac.⏊(())
      foo2setStar(b) should not be imageOf0
      foo2setStar(b) should not be imageOf1
    }
  }

  describe("Coproducts") {
    it("should give the expected construction for sets") {
      val foo = dot(0, 1)
      val bar = dot(a, b, c)
      val coproduct = foo + bar

      coproduct should have size 5
      foo +- bar should have (
        source(foo),
        target(coproduct),
        monic(true)
      )
      foo -+ bar should have (
        source(bar),
        target(coproduct),
        monic(true)
      )

      val tgt = dot("P", "Q", "R")
      val foo2target = arrow(foo, tgt)(0 -> "Q", 1 -> "P")
      val bar2target = arrow(bar, tgt)(a -> "R", b -> "P", c -> "Q")
      val sum = foo2target + bar2target

      sum should have (
        source(coproduct),
        target(tgt)
      )
      foo2target shouldBe (sum o (foo +- bar))
      bar2target shouldBe (sum o (foo -+ bar))
    }
  }

  describe("Universality of a predicate") {
    it("can be tested for sets") {
      val symbols = dot(a, b, c)

      symbols.universally { _ => true } shouldBe true
      symbols.universally { _ == a } shouldBe false
    }
  }

  describe("Epi-mono factorizations") {
    it("give the expected result for sets") {
      val symbols = dot(a, b, c)
      val numbers = dot(1, 2, 3, 4)
      val anArrow = arrow(symbols, numbers)(
        a -> 2,
        b -> 2,
        c -> 3
      )
      val (
        theEpic,
        theMonic
        ) : (
        Symbol > Int,
          Int > Int
        ) = anArrow.factorizeEpiMono
      theEpic.sanityTest
      theMonic.sanityTest
      theEpic shouldBe epic
      theMonic shouldBe monic
      (theMonic o theEpic) shouldBe anArrow
    }
  }

  describe("Quotients") {
    it("give the expected construction for sets") {
      val symbols = dot(a, b, c)
      val identifyBandC =
        relationFrom(
          symbols,
          a -> a,
          b -> b,
          c -> c,
          b -> c,
          c -> b
        )

      val quotient =
        symbols / identifyBandC

      val quotientArrow : Symbol > QUOTIENT[Symbol] =
        quotient.arrow
      quotientArrow.sanityTest
      quotientArrow.source shouldBe symbols
      quotientArrow shouldBe epic
      quotientArrow(a) should not be quotientArrow(b)
      quotientArrow(b) shouldBe quotientArrow(c)

      val quotientObject: DOT[QUOTIENT[Symbol]] =
        quotientArrow.target

      quotientObject.sanityTest
      quotientObject should have size 2

      val numbers = dot(1, 2, 3)
      val arrowToLift =
        arrow(symbols, numbers) (
          a -> 2,
          b -> 3,
          c -> 3
        )
      val lifted: QUOTIENT[Symbol] > Int =
        quotient.lift(arrowToLift)
      lifted.source shouldBe quotientObject
      lifted.target shouldBe numbers
      lifted o quotientArrow shouldBe arrowToLift
    }
  }

  describe("Coequalizers") {
    it("should give the expected construction for sets") {
      val symbols = dot(a, b)
      val numbers = dot(1, 2, 3, 4)

      val f1 = arrow(symbols, numbers)(
        a -> 1,
        b -> 3
      )
      val f2 = arrow(symbols, numbers)(
        a -> 2,
        b -> 4
      )
      val coequalizer =
        f1 =? f2
      val coequalizerArrow: Int > COEQUALIZER[Int] =
        coequalizer.arrow
      coequalizerArrow.source shouldBe numbers
      coequalizerArrow o f1 shouldBe (
        coequalizerArrow o f2
        )

      val booleans = dot(true, false)
      val otherEqualizer =
        arrow(numbers, booleans) (
          1 -> true,
          2 -> true,
          3 -> false,
          4 -> false
        )
      otherEqualizer o f1 shouldBe (
        otherEqualizer o f2
        )
      val factor: COEQUALIZER[Int] > Boolean =
        coequalizer.lift(otherEqualizer)
      factor o coequalizerArrow shouldBe otherEqualizer
    }

    it("should give the expected construction for sets (fast version)") {
      val symbols = dot(a, b)
      val numbers = dot(1, 2, 3, 4)

      val f1 = arrow(symbols, numbers)(
        a -> 1,
        b -> 3
      )
      val f2 = arrow(symbols, numbers)(
        a -> 2,
        b -> 4
      )
      val coequalizer =
        f1 =?! f2
      val coequalizerArrow: Int > COEQUALIZER[Int] =
        coequalizer.arrow
      coequalizerArrow.source shouldBe numbers
      coequalizerArrow o f1 shouldBe (
        coequalizerArrow o f2
        )

      val booleans = dot(true, false)
      val otherEqualizer =
        arrow(numbers, booleans) (
          1 -> true,
          2 -> true,
          3 -> false,
          4 -> false
        )
      otherEqualizer o f1 shouldBe (
        otherEqualizer o f2
        )
      val factor: COEQUALIZER[Int] > Boolean =
        coequalizer.lift(otherEqualizer)
      factor o coequalizerArrow shouldBe otherEqualizer
    }
  }

  describe("The projection operations *-, -*") {
    it("are correctly calculated for sets") {
      val symbols = dot(a, b, c)
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
      val symbols = dot(a, b, c)
      val numbers = dot(1, 2, 3)
      val f: Symbol > Int =
        arrow(symbols, numbers)(
          a -> 2, b -> 1, c -> 1
        )
      val h_f: (Int → Boolean) > (Symbol → Boolean) =
        h > f

      for {
        g <- elementsOf(numbers > h)
        symbol <- elementsOf(symbols)
      }
        h_f(g)(symbol) shouldBe g(f(symbol))
    }
  }

  describe("The covariant exponential functor _ ^ H") {
    it("behaves as expected") {
      val h = dot(true, false)
      val symbols = dot(a, b, c)
      val numbers = dot(1, 2, 3)
      val f: Symbol > Int =
        arrow(symbols, numbers)(
          a -> 2, b -> 1, c -> 1
        )
      val f_h: (Boolean → Symbol) > (Boolean → Int) =
        f > h

      for {
        g <- elementsOf(h > symbols)
        b <- elementsOf(h)
      }
        f_h(g)(b) shouldBe f(g(b))
    }
  }

  describe("To select subobjects") {
    it("you can use whereTrue") {
      val just2 =
        dot(1, 2, 3)(omega) {
          _ == 2
        }.whereTrue

      elementsOf(just2) shouldBe Seq(2)
    }

    it("you can use where") {
      val just2 =
        dot(1, 2, 3) where {
          _ == 2
        }

      elementsOf(just2) shouldBe Seq(2)
    }
  }

  describe("The intersection operator ⋀") {
    it("is correctly calculated for sets") {
      val symbols = dot(a, b, c)

      val intersection: Symbol > TRUTH =
        symbols.⋀(
          doubleCharacteristic(
            symbols
          )(
            Set(a, b),
            Set(a, c)
          )
        )
      intersection shouldBe
        symbols(omega) {
          _ == a
        }
    }
  }

  describe("Sections") {
    it("are detected correctly for sets") {
      val symbols = dot(a, b)
      val numbers = dot(1, 2, 3)
      val theSection: Symbol > Int =
        arrow(symbols, numbers)(
          a -> 2, b -> 1
        )
      theSection should be a section
      val nonsection: Symbol > Int =
        arrow(symbols, numbers)(
          a -> 2, b -> 2
        )
      nonsection should not be Matchers.a (section)
    }
  }

  describe("Retractions") {
    it("are detected correctly for sets") {
      val symbols = dot(a, b, c)
      val numbers = dot(1, 2)
      val section: Symbol > Int =
        arrow(symbols, numbers)(
          a -> 2, b -> 1, c -> 1
        )
      section should be a retraction
      val nonsection: Symbol > Int =
        arrow(symbols, numbers)(
          a -> 2, b -> 2, c -> 2
        )
      nonsection should not be Matchers.a (retraction)
    }
  }

  describe("Injective objects") {
    it("are detected properly for sets") {
      dot[VOID]() should not be injective
      dot(1) shouldBe injective
    }
  }

  describe("Minimality") {
    it("is detected properly for sets") {
      0 to 3 filter { n =>
        makeDot(0 until n).isMinimal
      } shouldBe Seq(1)
    }
    it("is detected properly for group actions") {
      val topos =
        ToposOfGroupActions of twoGroup

      topos.makeDot(
        twoGroup.regularAction
      ) shouldBe minimal
    }
  }

  describe("Simplicity") {
    it("is detected properly for sets") {
      0 to 3 filter { n =>
        makeDot(0 until n).isSimple
      } shouldBe Seq(2)
    }

    it("is detected properly for group actions") {
      val topos =
        ToposOfGroupActions of twoGroup

      topos.makeDot(
        twoGroup.regularAction
      ) shouldBe simple
    }
  }
 */
}

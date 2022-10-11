package com.fdilke.bewl2.topos

import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import com.fdilke.bewl2.sets.SetsUtilities.*

import scala.language.postfixOps

class AlgebraicMachinerySpec extends FunSuite:

//  implicit class ShouldBeToAssert[A](a: A):
//    inline def shouldBe(b: A): Unit =
//      assertEquals(a, b)
  extension[A](a: A)
    inline def shouldBe(b: A): Unit =
      assertEquals(a, b)

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos.StandardTermsAndOperators._
  import topos.NamedLaws._
  import topos._

  test("Simple and compound terms can describe their own free variables") {
    α.freeVariables shouldBe Seq(α)
    (α * β).freeVariables shouldBe Seq(α, β)
  }

  private val unstructuredSets = AlgebraicTheory()()
  private val theO: Unit => Boolean = makeNullaryOperator[Boolean](false)

  test("An evaluation context for no terms can evaluate constants, not variables") {
    val pointedSets = AlgebraicTheory(o)()
    val algebra = new pointedSets.Algebra[Boolean](o := theO)
    val context = algebra.EvaluationContext[Boolean](Seq())
    intercept[IllegalArgumentException] {
      context.evaluate(α)
    }
    intercept[IllegalArgumentException] {
      context.evaluateScalar(II)
    }
    assert { context.evaluate(o).asInstanceOf[Unit => Boolean] =!= theO }
  }

  test("An evaluation context for one term over an empty theory is just a uniproduct") {
    val algebra = new unstructuredSets.Algebra[Boolean]()
    val context: algebra.EvaluationContext[(Unit, Boolean)] =
      algebra.EvaluationContext[Boolean](Seq(α)).asInstanceOf[
        algebra.EvaluationContext[(Unit, Boolean)]
      ]
    val evalA: ((Unit, Boolean)) => Boolean = context.evaluate(α)
    assert { evalA.isIsoPlaceholderTrue }
  }

  test("An evaluation context for two terms over an empty theory is just a biproduct") {
    val algebra = new unstructuredSets.Algebra[Boolean]()
    val context: algebra.EvaluationContext[(Boolean, (Boolean, Unit))] =
      algebra.EvaluationContext[Boolean](Seq(α, β)).asInstanceOf[
        algebra.EvaluationContext[(Boolean, (Boolean, Unit))]
      ]

    val to_a: ((Boolean, (Boolean, Unit))) => Boolean = context.evaluate(α)
    val to_b: ((Boolean, (Boolean, Unit))) => Boolean = context.evaluate(β)

    assert {
      !( to_a =!= to_b )
    }
    (to_a x to_b).isIsoPlaceholderTrue shouldBe true
  }

  test("An evaluation context for one term can evaluate constants") {

    val pointedSets = AlgebraicTheory(o)()
    val algebra = new pointedSets.Algebra[Boolean](o := theO)
    val context = algebra.EvaluationContext[Boolean](Seq(α))

    val ppp: ((Boolean, Unit)) => Boolean = context.evaluate(o).asInstanceOf[((Boolean, Unit)) => Boolean]
    ppp((true, ()))
    val ppt: Boolean => Boolean = b => ppp(b, ())

    assert {
      ppt =!= ( theO o toUnit[Boolean] )
    }

    // TODO: clean this up! get rid of the casts.
  }

  test("An evaluation context can evaluate compound terms with unary operators") {
      implicit val carrier: Set[Int] = Set[Int](0, 1, 2)
      val theO: Unit => Int = makeNullaryOperator(0)
      val twiddle: Int => Int = makeUnaryOperator(
        0 -> 0,
        1 -> 2,
        2 -> 1
      )

      val pointedSetsWithOp: AlgebraicTheory[Unit] = AlgebraicTheory(o, ~)()
      val algebra: pointedSetsWithOp.Algebra[Int] =
        new pointedSetsWithOp.Algebra[Int](
          o := theO,
          StandardTermsAndOperators.~ := twiddle
        )
      val context: algebra.EvaluationContext[(Int, Unit)] = // or Unit, Int ??
        algebra.EvaluationContext(Seq(α)).asInstanceOf[
          algebra.EvaluationContext[(Int, Unit)]
        ]
      val interpretO: ((Int, Unit)) => Int = theO o toUnit[(Int, Unit)]
      val interpretα: ((Int, Unit)) => Int = context.evaluate(α)
      assert {
        ! ( interpretα =!= interpretO )
      }
      assert {
        context.evaluate(o) =!= interpretO
      }
      val minusO: Term[Principal] = ~o
      assert {
        context.evaluate(~o) =!= interpretO
      }
      assert {
        context.evaluate(α) =!= interpretα
      }
      assert {
        context.evaluate(~(~α)) =!= interpretα
      }
    }

/*
  test("An evaluation context can evaluate compound terms with binary operators") {
      val carrier = dot[String]("unit", "x")
      val theO = makeNullaryOperator(carrier, "unit")
      val plus = makeBinaryOperator(carrier,
        ("unit", "unit") -> "unit",
        ("x", "unit") -> "x",
        ("unit", "x") -> "x",
        ("x", "x") -> "x"
      )

      val pointedMagmas = AlgebraicTheory(o, $plus)()
      val algebra = new pointedMagmas.Algebra[String](carrier)(o := theO, $plus := plus)
      val context = algebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretα = context.evaluate(α)

      interpretα should not be interpretO
      context.evaluate(o) shouldBe interpretO
      context.evaluate(o + o) shouldBe interpretO
      context.evaluate(o + α) shouldBe interpretα
      context.evaluate(α + o) shouldBe interpretα
      context.evaluate(α + o) shouldBe interpretα
    }

  test("An evaluation context can evaluate compound terms with mixed binary operators") {
      val scalars = dot[Int](0, 1, 2)

      val carrier = dot[String]("o", "i")
      val theO = makeNullaryOperator(carrier, "o")
      val scalar1 = makeNullaryOperator(scalars, 2)
      val rightMultiply = bifunctionAsBiArrow(carrier, scalars, carrier)(
        Function untupled Map(
          ("o", 0) -> "o",
          ("i", 0) -> "o",
          ("o", 1) -> "i",
          ("i", 1) -> "i",
          ("o", 2) -> "i",
          ("i", 2) -> "o"
        )
      )

      val pointedWeakActs = AlgebraicTheoryWithScalars(scalars)(II := scalar1)(o, II, **)()
      val minimalAlgebra = new pointedWeakActs.Algebra[String](carrier)(
        o := theO, ** := rightMultiply
      )
      val context = minimalAlgebra.EvaluationContext(Seq(α))
      val interpretO = theO o context.root.toI
      val interpretI = makeNullaryOperator(carrier, "i") o context.root.toI

      context.evaluate(o) shouldBe interpretO
      context.evaluate(o ** II) shouldBe interpretI
      context.evaluate((α ** II) ** II) shouldBe context.evaluate(α)
    }

  test("An evaluation context can do operations on scalars") {
      val scalars = dot(0, 1, 2)
      val scalar1 = makeNullaryOperator(scalars, 1)
      val scalar2 = makeNullaryOperator(scalars, 2)
      val weakActsReferencingAMonoid = AlgebraicTheoryWithScalars(scalars)(II := scalar1)(**, ***)()
      val act = dot(x)
      val algebra = new weakActsReferencingAMonoid.Algebra[Symbol](act)(
        ** := (act x scalars).biArrow(act) { (_, _) => x },
        *** := bifunctionAsBiArrow(scalars) {
          (x, y) => (x + y) % 3
        }
      )

      algebra.EvaluationContext(Seq()).evaluateScalar(
        II *** II
      ) shouldBe scalar2

      algebra.EvaluationContext(Seq(α)).evaluate(
        α
      ) shouldBe (act *- I)

      algebra.EvaluationContext(Seq(α)).evaluate(
        α ** II
      ) shouldBe (act *- I)

      algebra.EvaluationContext(Seq(Ψ)).evaluateScalar(
        Ψ
      ) shouldBe (scalars *- I)

      algebra.EvaluationContext(Seq(Ψ)).evaluateScalar(
        ((Ψ *** II) *** II) *** II
      ) shouldBe (scalars *- I)
    }

  test("Algebraic theories can encapsulate commutative magmas") {
      val commutativeMagmas = AlgebraicTheory(*)(α * β := β * α)
      case class CommutativeMagma[T](
        override val carrier: DOT[T],
        op: BinaryOp[T]
      ) extends commutativeMagmas.Algebra[T](carrier)(* := op)

      val carrier = dot(true, false)
      val commutativeOp = bifunctionAsBiArrow(carrier) { _ & _}
      val nonCommutativeOp = bifunctionAsBiArrow(carrier) { _ & !_ }

     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean](carrier)().sanityTest
     }
     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean](carrier)($plus := commutativeOp).sanityTest
     }

      CommutativeMagma(carrier, commutativeOp).sanityTest
      intercept[IllegalArgumentException] {
        CommutativeMagma(carrier, nonCommutativeOp).sanityTest
      }
    }

  test("Algebraic theories can sanity-check their algebras for valid nullary operators") {
      val carrier = dot(0)
      val pointedSets = AlgebraicTheory(o)()
      val badZero =
        makeNullaryOperator(carrier, 1)

      intercept[IllegalArgumentException] {
        new pointedSets.Algebra[Int](
          carrier
        )(o := badZero).sanityTest
      }
    }

  test("Algebraic theories can sanity-check their algebras for valid unary operators") {
        val setsWithInvolution = AlgebraicTheory(~)(~(~α) := α)
        val carrier = dot(0)
        val invertBadRange =
          makeUnaryOperator(
            carrier,
            0 -> 1,
          )
        intercept[IllegalArgumentException] {
          new setsWithInvolution.Algebra[Int](
            carrier
          )(
            $tilde := invertBadRange
          ).sanityTest
        }
      }

  test("Algebraic theories can sanity-check their algebras for valid binary operators") {
      val magmas = AlgebraicTheory(*)()
      val carrier = dot(1)
      val combineBadRange =
        bifunctionAsBiArrow(
          carrier
        ) { _ + _ }
      intercept[IllegalArgumentException] {
        new magmas.Algebra[Int](
          carrier
        )(
          * := combineBadRange
        ).sanityTest
      }
    }

  test("Algebraic theories can sanity-check their algebras for scalar constants") {
      val scalars = dot(0)
      val badPointScalar =
        makeNullaryOperator(scalars, 1)
      val weakSetsOverAPointedSet =
        AlgebraicTheoryWithScalars(scalars)(II := badPointScalar)(II)()
      intercept[IllegalArgumentException] {
        new weakSetsOverAPointedSet.Algebra[String](
          dot("")
        )().sanityTest
      }
    }

  test("Algebraic theories can sanity-check their algebras for right scalar multiplications") {
      val scalars = dot(0)
      val carrier = dot(true)
      val badScalarRightMultiplication =
        bifunctionAsBiArrow(carrier, scalars, carrier) {
          Function untupled Map(
            (true, 0) -> false
          )
        }
      val weakActsOverASet =
        AlgebraicTheoryWithScalars(scalars)()(**)()
      intercept[IllegalArgumentException] {
        new weakActsOverASet.Algebra[Boolean](
          carrier
        )(
          ** := badScalarRightMultiplication
        ).sanityTest
      }
    }

  test("Algebraic theories can sanity-check their algebras for scalar multiplications; sad case") {
      val scalars = dot(0)
      val badScalarMultiplication =
        bifunctionAsBiArrow(scalars) {
          Function untupled Map(
            (0, 0) -> 1
          )
        }
      val weakActsOverASet =
        AlgebraicTheoryWithScalars(scalars)()(***)()
      intercept[IllegalArgumentException] {
        new weakActsOverASet.Algebra[Boolean](
          dot(true)
        )(
          *** := badScalarMultiplication
        ).sanityTest
      }
    }

  test("Algebraic theories can validate morphisms preserving unary operations") {
    val carrierStrings = dot[String]("+", "-")
    val minusStrings = makeUnaryOperator(carrierStrings,
      "+" -> "-",
      "-" -> "+"
    )
    val carrierInts = dot[Int](1, -1)
    val minusInts = makeUnaryOperator(carrierInts,
      1 -> -1,
      -1 -> 1
    )
    val wrongSource = dot[String]("*")
    val wrongTarget = dot[Int](0)

    val setsWithInvolution = AlgebraicTheory(~)(~(~α) := α)
    val algebraStrings = new setsWithInvolution.Algebra[String](carrierStrings)($tilde := minusStrings)
    val algebraInts = new setsWithInvolution.Algebra[Int](carrierInts)($tilde := minusInts)
    algebraStrings.sanityTest
    algebraInts.sanityTest

    val morphism = arrow(carrierStrings, carrierInts)("+" -> 1, "-" -> -1)
    val morphismWithWrongSource = arrow(wrongSource, carrierInts)("*" -> 1)
    morphismWithWrongSource.sanityTest
    val morphismWithWrongTarget = arrow(carrierStrings, wrongTarget)("+" -> 0, "-" -> 0)
    morphismWithWrongTarget.sanityTest
    setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphism) shouldBe true
    intercept[IllegalArgumentException] {
      setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphismWithWrongSource) shouldBe true
    }
    intercept[IllegalArgumentException] {
      setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphismWithWrongTarget) shouldBe true
    }

    val notAMorphism = arrow(carrierStrings, carrierInts)("+" -> 1, "-" -> 1)
    setsWithInvolution.isMorphism(algebraStrings, algebraInts, notAMorphism) shouldBe false
  }

  test("Algebraic theories can validate morphisms preserving constants") {
    val carrierInts = dot[Int](0, 1, 2)
    val pointInts = makeNullaryOperator(carrierInts, 0)
    val carrierStrings = dot[String]("samson", "delilah")
    val pointStrings = makeNullaryOperator(carrierStrings, "delilah")

    val pointedSets = AlgebraicTheory(o)()
    val algebraStrings = new pointedSets.Algebra[String](carrierStrings)(o := pointStrings)
    val algebraInts = new pointedSets.Algebra[Int](carrierInts)(o := pointInts)

    val morphism = arrow(carrierStrings, carrierInts)("samson" -> 1, "delilah" -> 0)
    val notAMorphism = arrow(carrierStrings, carrierInts)("samson" -> 1, "delilah" -> 1)

    pointedSets.isMorphism(algebraStrings, algebraInts, morphism) shouldBe true
    pointedSets.isMorphism(algebraStrings, algebraInts, notAMorphism) shouldBe false
  }

  test("Algebraic theories can validate morphisms preserving binary operations") {
    val carrier = dot[Int](0, 1, 2, 3)
    val multiplication = bifunctionAsBiArrow(carrier) {
      (x, y) => (x + y) % 4
    }

    val magmas = AlgebraicTheory(*)()
    val algebra = new magmas.Algebra[Int](carrier)(* := multiplication)

    val morphism = arrow(carrier, carrier)(0 -> 0, 1 -> 2, 2 -> 0, 3 -> 2)
    val notAMorphism = arrow(carrier, carrier)(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)

    magmas.isMorphism(algebra, algebra, morphism) shouldBe true
    magmas.isMorphism(algebra, algebra, notAMorphism) shouldBe false
  }

  test("Algebraic theories can validate morphisms preserving mixed operations with scalars") {
    val scalars = dot[Symbol](q)
    val pointScalar = makeNullaryOperator(scalars, q)
    val carrier = dot[Boolean](true, false)
    val multiplication = bifunctionAsBiArrow(carrier, scalars, carrier) {
      Function untupled Map(
        (true, q) -> false,
        (false, q) -> true
      )
    }

    val weakActsOverAPointedMagma = AlgebraicTheoryWithScalars(scalars)(II := pointScalar)(II, **)()
    val algebra = new weakActsOverAPointedMagma.Algebra[Boolean](carrier)(** := multiplication)
    algebra.sanityTest

    val morphism = arrow(carrier, carrier)(true -> false, false -> true)
    val notAMorphism = arrow(carrier, carrier)(true -> true, false -> true)

    weakActsOverAPointedMagma.isMorphism(algebra, algebra, morphism) shouldBe true
    weakActsOverAPointedMagma.isMorphism(algebra, algebra, notAMorphism) shouldBe false
  }

  test("Algebraic theories support named laws with error reporting when they fail") {
    import NamedLaws._
    val carrier = dot[Int](1, -1)
    val minusGood = makeUnaryOperator(carrier,
      1 -> -1,
      -1 -> 1
    )
    val minusBad = makeUnaryOperator(carrier,
      1 -> 1,
      -1 -> 1
    )

    val setsWithInvolution = AlgebraicTheory(~)(
      "involutive" law (~(~α) := α)
    )
    new setsWithInvolution.Algebra[Int](carrier)($tilde := minusGood).sanityTest
    intercept[IllegalArgumentException] {
      new setsWithInvolution.Algebra[Int](carrier)($tilde := minusBad).sanityTest
    }.getMessage should include("involutive")
  }

  test("Algebraic theories can verify additional laws once an algebra is constructed") {
    import NamedLaws._
    val carrier = dot[Int](1, -1)
    val minus = makeUnaryOperator(carrier,
      1 -> -1,
      -1 -> 1
    )

    val setsWithInvolution = AlgebraicTheory(~)(
      "involutive" law (~(~α) := α)
    )
    val algebra = new setsWithInvolution.Algebra[Int](carrier)($tilde := minus)
    algebra.sanityTest

    algebra.satisfies(
      ~α := α
    ) shouldBe false
    algebra.satisfies(
      ~α := ~(~(~α))
    ) shouldBe true
  }

  test("Algebraic theories can be extended by adding new constants/operations, and laws ") {
      val magmas = AlgebraicTheory(*)()
      val commutativeMagmasWith1 =
        magmas.extend(ι)(
          "commutative" law( α * β := β * α ),
          "unit" law( α * ι := α )
        )
      case class CommutativeMagmaWith1[T](
        override val carrier: DOT[T],
        unit: NullaryOp[T],
        op: BinaryOp[T]
      ) extends commutativeMagmasWith1.Algebra[T](carrier)(
        ι := unit,
        * := op
      )

      val carrier = dot(true, false)
      val commutativeOp = bifunctionAsBiArrow(carrier) { _ & _}
      val nonCommutativeOp = bifunctionAsBiArrow(carrier) { _ & !_ }
      val okUnit = makeNullaryOperator(carrier, true)
      val notOkUnit = makeNullaryOperator(carrier, false)

      new CommutativeMagmaWith1[Boolean](
        carrier,
        okUnit,
        commutativeOp
      ).sanityTest

      intercept[IllegalArgumentException] {
        new CommutativeMagmaWith1[Boolean](
          carrier,
          okUnit,
          nonCommutativeOp
        ).sanityTest
      }.getMessage shouldBe "commutative law failed"

      intercept[IllegalArgumentException] {
        new CommutativeMagmaWith1[Boolean](
          carrier,
          notOkUnit,
          commutativeOp
        ).sanityTest
      }.getMessage shouldBe "unit law failed"
  }

  test("Algebraic theories upport binary multiplication of their algebras") {
    def integersMod(n: Int) = {
      val carrier = makeDot(0 until n)
      new Group[Int](
        carrier,
        makeNullaryOperator(carrier, 0),
        bifunctionAsBiArrow(carrier) {
          (x: Int, y:Int) => (x + y) % n
        },
        functionAsArrow(
          carrier,
          carrier,
          i => (n - i) % n
        )
      )
    }
    val c2 = integersMod(2)
    val c3 = integersMod(3)
    val c6 = integersMod(6)
    c2.sanityTest
    c3.sanityTest
    c6.sanityTest
    val c2xc3: Group[Int x Int] =
      c2 x c3
    c2xc3.sanityTest
    val product =
      c2.carrier x c3.carrier
    c2xc3.carrier shouldBe ( product )
    c6.carrier(c2xc3.carrier) {
      a => product.pair(a % 2, a % 3)
    } shouldBe iso

    groups.isMorphism(
      c2xc3,
      c2,
      product.π0
    ) shouldBe true
    groups.isMorphism(
      c2xc3,
      c3,
      product.π1
    ) shouldBe true
  }

  test("Algebraic theories support binary multiplication of their algebras, even with scalar extensions") {
    import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
    import monoidOf3.regularAction

    val barDot = dot("x", "y")
    val scalarMultiply: (String, Symbol) => String =
      (s, m) => monoidOf3.multiply(Symbol(s), m).name

    val bar = monoidOf3.action(barDot)(scalarMultiply)

    val product: monoidOf3.Action[String x Symbol] =
      bar x regularAction
    val underlyingProduct = barDot x regularAction.actionCarrier
    product.sanityTest
    product.carrier shouldBe underlyingProduct
    product.operatorAssignments.lookup(II).get(()) shouldEqual i
    monoidOf3.actions.isMorphism[String x Symbol, String](
      product,
      bar,
      underlyingProduct.π0
    )
    monoidOf3.actions.isMorphism[String x Symbol, Symbol](
      product,
      regularAction,
      underlyingProduct.π1
    )

    val operatorsUsed =
      product.operatorAssignments.assignments map {
        _.operator
      }

    operatorsUsed.distinct.size shouldBe operatorsUsed.size
  }


*/
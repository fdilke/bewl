package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.Direction.*
import com.fdilke.bewl2.utility.{Direction, RichFunSuite}

import scala.Function.tupled
import scala.language.postfixOps

class AlgebraicTheoriesSpec extends RichFunSuite:

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos.*
  import topos.StandardTermsAndOperators.*

  test("Simple and compound terms can describe their own free variables") {
    α.freeVariables is Seq(α)
    (α * β).freeVariables is Seq(α, β)
  }

  private val unstructuredSets = AlgebraicTheory()()
  private val theO: Unit => Boolean = makeNullaryOperator[Boolean](false)

  test("An evaluation context for no terms can evaluate constants, not variables") {
    val pointedSets = AlgebraicTheory(o)()
    val algebra = new pointedSets.Algebra[Boolean](o := theO)
    val context: algebra.EvaluationContext[Unit] =
      algebra.EvaluationContext(Seq()).asInstanceOf[
        algebra.EvaluationContext[Unit]
      ]
    intercept[IllegalArgumentException] {
      context.evaluatePrincipal(α)
    }
    intercept[IllegalArgumentException] {
      context.evaluateScalar(II)
    }
    context.evaluatePrincipal(o) isArrow theO
  }

  test("An evaluation context for one term over an empty theory is just a uniproduct") {
    val algebra = new unstructuredSets.Algebra[Boolean]()
    val context: algebra.EvaluationContext[Boolean] =
      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[Boolean]
      ]
    val evalA: Boolean => Boolean = context.evaluatePrincipal(α)

    evalA isArrow id[Boolean]
    evalA.isIso is true
  }

  test("An evaluation context for two terms over an empty theory is just a biproduct") {
    val algebra = new unstructuredSets.Algebra[Boolean]()
    val context: algebra.EvaluationContext[(Boolean, Boolean)] =
      algebra.EvaluationContext(Seq(α, β)).asInstanceOf[
        algebra.EvaluationContext[(Boolean, Boolean)]
      ]

    val to_a: ((Boolean, Boolean)) => Boolean = context.evaluatePrincipal(α)
    val to_b: ((Boolean, Boolean)) => Boolean = context.evaluatePrincipal(β)

    to_a isNotArrow to_b
    (to_a x to_b).isIso is true
  }

  test("An evaluation context for one term can evaluate constants") {

    val pointedSets = AlgebraicTheory(o)()
    val algebra = new pointedSets.Algebra[Boolean](o := theO)
    val context: algebra.EvaluationContext[Boolean] =
      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[Boolean]
      ]

    theO o toUnit[Boolean] isArrow {
      context.evaluatePrincipal(o)
    }
  }

  test("An evaluation context can evaluate compound terms with unary operators") {
    withDot(Set[Int](0, 1, 2)) {
      val theO: Unit => Int = makeNullaryOperator(0)
      val twiddle: Int => Int = makeUnaryOperator(
        0 -> 0,
        1 -> 2,
        2 -> 1
      )

      val pointedSetsWithOp: Variety = AlgebraicTheory(o, ~)()
      val algebra: pointedSetsWithOp.Algebra[Int] =
        new pointedSetsWithOp.Algebra[Int](
          (~) := twiddle,
          o := theO
        )
      val context: algebra.EvaluationContext[Int] =
        algebra.EvaluationContext(Seq(α)).asInstanceOf[
          algebra.EvaluationContext[Int]
        ]
      val interpretO: Int => Int = theO o toUnit[Int]
      val interpretα: Int => Int = context.evaluatePrincipal(α)
      interpretα isNotArrow interpretO
      context.evaluatePrincipal(o) isArrow interpretO
      context.evaluatePrincipal(~o) isArrow interpretO
      context.evaluatePrincipal(α) isArrow interpretα
      context.evaluatePrincipal(~(~α)) isArrow interpretα
    }
  }

  test("An evaluation context can evaluate compound terms with binary operators") {
    withDot(Set[String]("unit", "x")) {
      val theO: Unit => String = makeNullaryOperator[String]("unit")
      val plus = makeBinaryOperator[String](
        ("unit", "unit") -> "unit",
        ("x", "unit") -> "x",
        ("unit", "x") -> "x",
        ("x", "x") -> "x"
      )

      val pointedMagmas = AlgebraicTheory(o, +)()
      val algebra = new pointedMagmas.Algebra[String]((+) := plus, o := theO)
      val context: algebra.EvaluationContext[String] =
        algebra.EvaluationContext(Seq(α)).asInstanceOf[
          algebra.EvaluationContext[String]
        ]
      val interpretO: String => String = theO o toUnit[String]
      val interpretα: String => String = context.evaluatePrincipal(α)

      interpretα isNotArrow interpretO
      context.evaluatePrincipal(o) isArrow interpretO
      context.evaluatePrincipal(o + o) isArrow interpretO
      context.evaluatePrincipal(o + α) isArrow interpretα
      context.evaluatePrincipal(α + o) isArrow interpretα
      context.evaluatePrincipal(α + o) isArrow interpretα
    }
  }

  test("An evaluation context can evaluate compound terms with mixed binary operators") {
    withDots(
      Set[Int](0, 1, 2),
      Set[String]("o", "i")
    ) {
      val theO: Unit => String = makeNullaryOperator[String]("o")
      val scalar1: Unit => Int = makeNullaryOperator[Int](2)
      val rightMultiply: BiArrow[String, Int, String] = Map(
        ("o", 0) -> "o",
        ("i", 0) -> "o",
        ("o", 1) -> "i",
        ("i", 1) -> "i",
        ("o", 2) -> "i",
        ("i", 2) -> "o"
      )

      val pointedWeakActs: AlgebraicTheory[Int] =
        AlgebraicTheoryWithScalars[Int](II := scalar1)(o, II, **)()

      val minimalAlgebra = new pointedWeakActs.Algebra[String](
        o := theO,
        ** := rightMultiply
      )
      val context: minimalAlgebra.EvaluationContext[String] =
        minimalAlgebra.EvaluationContext(Seq(α)).asInstanceOf[
          minimalAlgebra.EvaluationContext[String]
        ]
      val interpretO: String => String = theO o toUnit[String]
      val interpretI: String => String = makeNullaryOperator[String]("i") o toUnit[String]

      context.evaluatePrincipal(o) isArrow interpretO
      context.evaluatePrincipal(o ** II) isArrow interpretI
      context.evaluatePrincipal((α ** II) ** II) isArrow context.evaluatePrincipal(α)
    }
  }

  test("An evaluation context can do operations on scalars") {
    withDots(
      Set[Int](0, 1, 2),
      Set(Symbol("x"))
    ) {
      val scalar1: Unit => Int = makeNullaryOperator[Int](1)
      val scalar2: Unit => Int = makeNullaryOperator[Int](2)
      val weakActsReferencingAMonoid: AlgebraicTheory[Int] =
        AlgebraicTheoryWithScalars[Int](
          II := scalar1,
          *** := { (xy: (Int, Int)) =>
            val (x, y) = xy
            (x + y) % 3
          }
        )(**, ***)()
      val algebra: weakActsReferencingAMonoid.Algebra[Symbol] =
        new weakActsReferencingAMonoid.Algebra[Symbol](
          ** := { (as: (Symbol, Int)) => Symbol("x") }
        )

      algebra.EvaluationContext(Seq()).asInstanceOf[
        algebra.EvaluationContext[Unit]
      ].evaluateScalar(
        II *** II
      ) isArrow scalar2

      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[Symbol]
      ].evaluatePrincipal(
        α
      ) isArrow id[Symbol]

      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[Symbol]
      ].evaluatePrincipal(
        α ** II
      ) isArrow id[Symbol]

      algebra.EvaluationContext(Seq(Ψ)).asInstanceOf[
        algebra.EvaluationContext[Int]
      ].evaluateScalar(
        Ψ
      ) isArrow id[Int]

      algebra.EvaluationContext(Seq(Ψ)).asInstanceOf[
        algebra.EvaluationContext[Int]
      ].evaluateScalar(
        ((Ψ *** II) *** II) *** II
      ) isArrow id[Int]
    }
  }

  test("Algebraic theories can encapsulate commutative magmas") {
    val commutativeMagmas = AlgebraicTheory(*)(α * β := β * α)
    case class CommutativeMagma[T: Dot](
      op: BinaryOp[T]
    ) extends commutativeMagmas.Algebra[T](* := op)

    val commutativeOp: ((Boolean, Boolean)) => Boolean = tupled { _ & _}
    val nonCommutativeOp: ((Boolean, Boolean)) => Boolean = tupled { _ & !_ }

     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean]().sanityTest
     }
     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean]((+) := commutativeOp).sanityTest
     }

    CommutativeMagma[Boolean](commutativeOp).sanityTest
    intercept[IllegalArgumentException] {
      CommutativeMagma[Boolean](nonCommutativeOp).sanityTest
    }
  }

  test("Algebraic theories can sanity-check their algebras for valid nullary operators") {
    withDot(Set[Int](0)) {
      val pointedSets = AlgebraicTheory(o)()
      val badZero: Unit => Int = makeNullaryOperator[Int](1)

      intercept[IllegalArgumentException] {
        new pointedSets.Algebra[Int](o := badZero).sanityTest
      }
    }
  }

  test("Algebraic theories can sanity-check their algebras for valid unary operators") {
    val setsWithInvolution = AlgebraicTheory(~)(~(~α) := α)
    withDot(Set[Int](0)) {
      val invertBadRange: Int => Int = makeUnaryOperator[Int](0 -> 1)
      intercept[IllegalArgumentException] {
        new setsWithInvolution.Algebra[Int](
          (~) := invertBadRange
        ).sanityTest
      }
    }
  }

  test("Algebraic theories can sanity-check their algebras for valid binary operators") {
    val magmas = AlgebraicTheory(*)()
    withDot(Set[Int](1)) {
      val combineBadRange: ((Int, Int)) => Int = tupled {
        _ + _
      }

      intercept[IllegalArgumentException] {
        new magmas.Algebra[Int](
          * := combineBadRange
        ).sanityTest
      }
    }
  }

  test("Algebraic theories can sanity-check their algebras for scalar constants") {
    withDots(
      Set[Int](0),
      Set[String]("x")
    ) {
      val badPointScalar: Unit => Int = makeNullaryOperator[Int](1)

      val weakSetsOverAPointedSet =
        AlgebraicTheoryWithScalars[Int](II := badPointScalar)(II)()
      intercept[IllegalArgumentException] {
        new weakSetsOverAPointedSet.Algebra[String]().sanityTest
      }
    }
  }

  test("Algebraic theories can sanity-check their algebras for right scalar multiplications") {
    withDots(
      Set[Int](0),
      Set[Direction](Up)
    ) {
      val badScalarRightMultiplication: ((Direction, Int)) => Direction =
        Map(
          (Up, 0) -> Down
        )

      val weakActsOverASet =
        AlgebraicTheoryWithScalars[Int]()(**)()

      intercept[IllegalArgumentException] {
        new weakActsOverASet.Algebra[Direction](
          ** := badScalarRightMultiplication
        ).sanityTest
      }
    }
  }

  test("Algebraic theories can sanity-check their algebras for scalar multiplications; sad case") {
    withDots(
      Set[Int](0),
      Set[String]("x")
    ) {
      val badScalarMultiplication: ((Int, Int)) => Int =
        Map(
          (0, 0) -> 1
        )

      val weakActsOverASet =
        AlgebraicTheoryWithScalars[Int](*** := badScalarMultiplication)(***)()
      intercept[IllegalArgumentException] {
        new weakActsOverASet.Algebra[String]().sanityTest
      }
    }
  }

  test("Algebraic theories can validate morphisms preserving unary operations") {
    withDots(
      Set[String]("+", "-"),
      Set[Int](1, -1)
    ) {
      val minusStrings: String => String =
        makeUnaryOperator[String](values =
          "+" -> "-",
          "-" -> "+"
        )
      val minusInts: Int => Int =
        makeUnaryOperator[Int](values =
          1 -> -1,
          -1 -> 1
        )

      val setsWithInvolution: Variety =
        AlgebraicTheory(~)(~(~α) := α)
      implicit val algebraStrings: setsWithInvolution.Algebra[String] =
        new setsWithInvolution.Algebra[String](
          (~) := minusStrings
        )
      implicit val algebraInts: setsWithInvolution.Algebra[Int] =
        new setsWithInvolution.Algebra[Int](
          (~) := minusInts
        )
      algebraStrings.sanityTest
      algebraInts.sanityTest

      val morphism: String => Int = Map("+" -> 1, "-" -> -1)
      setsWithInvolution.isMorphism(morphism) is true

      val notAMorphism: String => Int = Map("+" -> 1, "-" -> 1)
      setsWithInvolution.isMorphism(notAMorphism) is false
    }
  }

  test("Algebraic theories can validate morphisms preserving constants") {
    withDots(
      Set[Int](0, 1, 2),
      Set[String]("samson", "delilah")
    ) {
      val pointInts: Unit => Int = makeNullaryOperator(0)
      val pointStrings: Unit => String = makeNullaryOperator("delilah")

      val pointedSets: Variety = AlgebraicTheory(o)()
      implicit val algebraStrings: pointedSets.Algebra[String] =
        new pointedSets.Algebra[String](o := pointStrings)
      implicit val algebraInts: pointedSets.Algebra[Int] =
        new pointedSets.Algebra[Int](o := pointInts)

      val morphism: String => Int = Map("samson" -> 1, "delilah" -> 0)
      val notAMorphism: String => Int = Map("samson" -> 1, "delilah" -> 1)

      pointedSets.isMorphism(morphism) is true
      pointedSets.isMorphism(notAMorphism) is false
    }
  }

  test("Algebraic theories can validate morphisms preserving binary operations") {
    withDot(Set[Int](0, 1, 2, 3)) {
      val multiplication: ((Int, Int)) => Int = tupled {
        (x, y) => (x + y) % 4
      }

      val magmas: Variety =
        AlgebraicTheory(*)()
      implicit val algebra: magmas.Algebra[Int] =
        new magmas.Algebra[Int](* := multiplication)

      val morphism: Int => Int = Map(0 -> 0, 1 -> 2, 2 -> 0, 3 -> 2)
      val notAMorphism: Int => Int = Map(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)

      magmas.isMorphism(morphism) is true
      magmas.isMorphism(notAMorphism) is false
    }
  }

  test("Algebraic theories can validate morphisms preserving mixed operations with scalars") {
    val q = Symbol("q")
    withDot(Set[Symbol](q)) {
      val pointScalar: Unit => Symbol = makeNullaryOperator[Symbol](q)
      val multiplication: ((Boolean, Symbol)) => Boolean = Map(
        (true, q) -> false,
        (false, q) -> true
      )

      val weakActsOverAPointedMagma: AlgebraicTheory[Symbol] =
        AlgebraicTheoryWithScalars[Symbol](II := pointScalar)(II, **)()
      implicit val algebra: weakActsOverAPointedMagma.Algebra[Boolean] =
        new weakActsOverAPointedMagma.Algebra[Boolean](** := multiplication)
      algebra.sanityTest

      val morphism: Boolean => Boolean = Map(true -> false, false -> true)
      val notAMorphism: Boolean => Boolean = Map(true -> true, false -> true)

      weakActsOverAPointedMagma.isMorphism(morphism) is true
      weakActsOverAPointedMagma.isMorphism(notAMorphism) is false
    }
  }

  test("Algebraic theories support named laws with error reporting when they fail") {
    withDot(Set[Int](1, -1)) {
      val minusGood: Int => Int = Map(
        1 -> -1,
        -1 -> 1
      )
      val minusBad: Int => Int = Map(
        1 -> 1,
        -1 -> 1
      )

      val setsWithInvolution = AlgebraicTheory(~)(
        "involutive" law (~(~α) := α)
      )
      new setsWithInvolution.Algebra[Int]((~) := minusGood).sanityTest
      intercept[IllegalArgumentException] {
        new setsWithInvolution.Algebra[Int]((~) := minusBad).sanityTest
      }.getMessage.contains("involutive") is true
    }
  }

  test("Algebraic theories can verify additional laws once an algebra is constructed") {
    withDot(Set[Int](1, -1)) {
      val minus: Int => Int = Map(
        1 -> -1,
        -1 -> 1
      )

      val setsWithInvolution = AlgebraicTheory(~)(
        "involutive" law (~(~α) := α)
      )
      val algebra = new setsWithInvolution.Algebra[Int]((~) := minus)
      algebra.sanityTest

      algebra.satisfies(
        ~α := α
      ) is false
      algebra.satisfies(
        ~α := ~(~(~α))
      ) is true
    }
  }

  test("Algebraic theories can be extended by adding new constants/operations, and laws ") {
    val magmas = AlgebraicTheory(*)()
    val commutativeMagmasWith1 =
      magmas.extend(ι)(
        "commutative" law( α * β := β * α ),
        "unit" law( α * ι := α )
      )
    case class CommutativeMagmaWith1[T: Dot](
      unit: NullaryOp[T],
      op: BinaryOp[T]
    ) extends commutativeMagmasWith1.Algebra[T](
      ι := unit,
      * := op
    )

    val commutativeOp: ((Boolean, Boolean)) => Boolean = tupled { _ & _}
    val nonCommutativeOp: ((Boolean, Boolean)) => Boolean = tupled{ _ & !_ }
    val okUnit: Unit => Boolean = makeNullaryOperator(true)
    val notOkUnit: Unit => Boolean = makeNullaryOperator(false)

    new CommutativeMagmaWith1[Boolean](
      okUnit,
      commutativeOp
    ).sanityTest

    intercept[IllegalArgumentException] {
      new CommutativeMagmaWith1[Boolean](
        okUnit,
        nonCommutativeOp
      ).sanityTest
    }.getMessage is "commutative law failed"

    intercept[IllegalArgumentException] {
      new CommutativeMagmaWith1[Boolean](
        notOkUnit,
        commutativeOp
      ).sanityTest
    }.getMessage is "unit law failed"
  }


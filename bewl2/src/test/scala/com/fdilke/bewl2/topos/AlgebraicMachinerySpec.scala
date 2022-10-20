package com.fdilke.bewl2.topos

import com.fdilke.bewl2.algebra.Principal
import com.fdilke.bewl2.sets.Sets
import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.bewl2.utility.Direction
import Direction.*

import scala.language.postfixOps

class AlgebraicMachinerySpec extends FunSuite:

  extension[A](a: A)
    inline def is(b: A): Unit =
      assertEquals(a, b)

  private val topos = com.fdilke.bewl2.sets.Sets
  import topos.StandardTermsAndOperators._
  import topos._

  extension[A: Set, B: Set](arrow: A => B)
    inline def isArrow(arrow2: A => B): Unit =
      assert(arrow =!= arrow2)
    inline def isNotArrow(arrow2: A => B): Unit =
      assert( !( arrow =!= arrow2) )

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
    val context: algebra.EvaluationContext[(Unit, Boolean)] =
      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[(Unit, Boolean)]
      ]
    val evalA: ((Unit, Boolean)) => Boolean = context.evaluatePrincipal(α)
    evalA.isIsoPlaceholderTrue is true
  }

  test("An evaluation context for two terms over an empty theory is just a biproduct") {
    val algebra = new unstructuredSets.Algebra[Boolean]()
    val context: algebra.EvaluationContext[(Boolean, (Boolean, Unit))] =
      algebra.EvaluationContext(Seq(α, β)).asInstanceOf[
        algebra.EvaluationContext[(Boolean, (Boolean, Unit))]
      ]

    val to_a: ((Boolean, (Boolean, Unit))) => Boolean = context.evaluatePrincipal(α)
    val to_b: ((Boolean, (Boolean, Unit))) => Boolean = context.evaluatePrincipal(β)

    to_a isNotArrow to_b
    (to_a x to_b).isIsoPlaceholderTrue is true
  }

  test("An evaluation context for one term can evaluate constants") {

    val pointedSets = AlgebraicTheory(o)()
    val algebra = new pointedSets.Algebra[Boolean](o := theO)
    val context: algebra.EvaluationContext[(Boolean, Unit)] =
      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[(Boolean, Unit)]
      ]

    theO o toUnit[Boolean] isArrow {
      context.evaluatePrincipal(o)(_, ())
    }
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
      val interpretα: ((Int, Unit)) => Int = context.evaluatePrincipal(α)
      interpretα isNotArrow interpretO
      context.evaluatePrincipal(o) isArrow interpretO
      val minusO: Term[Principal] = ~o
      context.evaluatePrincipal(~o) isArrow interpretO
      context.evaluatePrincipal(α) isArrow interpretα
      context.evaluatePrincipal(~(~α)) isArrow interpretα
    }

  test("An evaluation context can evaluate compound terms with binary operators") {
    implicit val carrier: Set[String] = Set[String]("unit", "x")
    val theO: Unit => String = makeNullaryOperator[String]("unit")
    val plus = makeBinaryOperator[String](
      ("unit", "unit") -> "unit",
      ("x", "unit") -> "x",
      ("unit", "x") -> "x",
      ("x", "x") -> "x"
    )

    val pointedMagmas = AlgebraicTheory(o, +)()
    val algebra = new pointedMagmas.Algebra[String](o := theO, StandardTermsAndOperators.+ := plus)
    val context: algebra.EvaluationContext[(String, Unit)] =
      algebra.EvaluationContext(Seq(α)).asInstanceOf[
        algebra.EvaluationContext[(String, Unit)]
      ]
    val interpretO: ((String, Unit)) => String = theO o toUnit[(String, Unit)]
    val interpretα: ((String, Unit)) => String = context.evaluatePrincipal(α)

    interpretα isNotArrow interpretO
    context.evaluatePrincipal(o) isArrow interpretO
    context.evaluatePrincipal(o + o) isArrow interpretO
    context.evaluatePrincipal(o + α) isArrow interpretα
    context.evaluatePrincipal(α + o) isArrow interpretα
    context.evaluatePrincipal(α + o) isArrow interpretα
  }

  test("An evaluation context can evaluate compound terms with mixed binary operators") {
    implicit val scalars: Set[Int] = Set[Int](0, 1, 2)
    implicit val carrier: Set[String] = Set[String]("o", "i")

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
    val context: minimalAlgebra.EvaluationContext[(String, Unit)] =
      minimalAlgebra.EvaluationContext(Seq(α)).asInstanceOf[
        minimalAlgebra.EvaluationContext[(String, Unit)]
      ]
    val interpretO: ((String, Unit)) => String = theO o toUnit[(String, Unit)]
    val interpretI: ((String, Unit)) => String = makeNullaryOperator[String]("i") o toUnit[(String, Unit)]

    context.evaluatePrincipal(o) isArrow interpretO
    context.evaluatePrincipal(o ** II) isArrow interpretI
    context.evaluatePrincipal((α ** II) ** II) isArrow context.evaluatePrincipal(α)
  }

  test("An evaluation context can do operations on scalars") {
    implicit val scalars: Set[Int] = Set(0, 1, 2)
    val scalar1: Unit => Int = makeNullaryOperator[Int](1)
    val scalar2: Unit => Int = makeNullaryOperator[Int](2)
    implicit val act: Set[Symbol] = Set(Symbol("x"))
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
      algebra.EvaluationContext[(Symbol, Unit)]
    ].evaluatePrincipal(
      α
    ) isArrow π0[Symbol, Unit] // (act *- I)

    algebra.EvaluationContext(Seq(α)).asInstanceOf[
      algebra.EvaluationContext[(Symbol, Unit)]
    ].evaluatePrincipal(
      α ** II
    ) isArrow π0[Symbol, Unit]

    algebra.EvaluationContext(Seq(Ψ)).asInstanceOf[
      algebra.EvaluationContext[(Int, Unit)]
    ].evaluateScalar(
      Ψ
    ) isArrow π0[Int, Unit]

    algebra.EvaluationContext(Seq(Ψ)).asInstanceOf[
      algebra.EvaluationContext[(Int, Unit)]
    ].evaluateScalar(
      ((Ψ *** II) *** II) *** II
    ) isArrow π0[Int, Unit]
  }

  test("Algebraic theories can encapsulate commutative magmas") {
    val commutativeMagmas = AlgebraicTheory(*)(α * β := β * α)
    case class CommutativeMagma[T: Set](
      op: BinaryOp[T]
    ) extends commutativeMagmas.Algebra[T](* := op)

    val commutativeOp: ((Boolean, Boolean)) => Boolean = Function.tupled { _ & _}
    val nonCommutativeOp: ((Boolean, Boolean)) => Boolean = Function.tupled { _ & !_ }

     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean]().sanityTest
     }
     intercept[IllegalArgumentException] {
        new commutativeMagmas.Algebra[Boolean](StandardTermsAndOperators.+ := commutativeOp).sanityTest
     }

    CommutativeMagma[Boolean](commutativeOp).sanityTest
    intercept[IllegalArgumentException] {
      CommutativeMagma[Boolean](nonCommutativeOp).sanityTest
    }
  }

  test("Algebraic theories can sanity-check their algebras for valid nullary operators") {
    implicit val carrier: Set[Int] = Set(0)
    val pointedSets = AlgebraicTheory(o)()
    val badZero: Unit => Int = makeNullaryOperator[Int](1)

    intercept[IllegalArgumentException] {
      new pointedSets.Algebra[Int](o := badZero).sanityTest
    }
  }

  test("Algebraic theories can sanity-check their algebras for valid unary operators") {
    val setsWithInvolution = AlgebraicTheory(~)(~(~α) := α)
    implicit val carrier: Set[Int] = Set(0)
    val invertBadRange: Int => Int = makeUnaryOperator[Int](0 -> 1)
    intercept[IllegalArgumentException] {
      new setsWithInvolution.Algebra[Int](
        StandardTermsAndOperators.~ := invertBadRange
      ).sanityTest
    }
  }

  test("Algebraic theories can sanity-check their algebras for valid binary operators") {
    val magmas = AlgebraicTheory(*)()
    implicit val carrier: Set[Int] = Set(1)
    val combineBadRange: ((Int, Int)) => Int = Function.tupled { _ + _ }

    intercept[IllegalArgumentException] {
      new magmas.Algebra[Int](
        * := combineBadRange
      ).sanityTest
    }
  }

  test("Algebraic theories can sanity-check their algebras for scalar constants") {
    implicit val scalars: Set[Int] = Set(0)
    implicit val carrier: Set[String] = Set("x")
    val badPointScalar: Unit => Int = makeNullaryOperator[Int](1)

    val weakSetsOverAPointedSet =
      AlgebraicTheoryWithScalars[Int](II := badPointScalar)(II)()
    intercept[IllegalArgumentException] {
      new weakSetsOverAPointedSet.Algebra[String]().sanityTest
    }
  }

  test("Algebraic theories can sanity-check their algebras for right scalar multiplications") {
    implicit val scalars: Set[Int] = Set(0)
    implicit val carrier: Set[Direction] = Set(Up)
    val badScalarRightMultiplication : ((Direction, Int)) => Direction =
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

  test("Algebraic theories can sanity-check their algebras for scalar multiplications; sad case") {
    implicit val scalars: Set[Int] = Set(0)
      val badScalarMultiplication: ((Int, Int)) => Int =
        Map(
            (0, 0) -> 1
          )
      implicit val carrier: Set[String] = Set("x")

      val weakActsOverASet =
        AlgebraicTheoryWithScalars[Int](*** := badScalarMultiplication)(***)()
      intercept[IllegalArgumentException] {
        new weakActsOverASet.Algebra[String]().sanityTest
      }
    }

  test("Algebraic theories can validate morphisms preserving unary operations") {
    implicit val carrierString: Set[String] = Set("+", "-")
    val minusStrings: String => String =
      makeUnaryOperator[String](values =
        "+" -> "-",
        "-" -> "+"
      )
    implicit val carrierInts: Set[Int] = Set(1, -1)
    val minusInts: Int => Int =
      makeUnaryOperator[Int](values =
        1 -> -1,
        -1 -> 1
      )

    val setsWithInvolution = AlgebraicTheory(~)(~(~α) := α)
    val algebraStrings = new setsWithInvolution.Algebra[String](
      StandardTermsAndOperators.~ := minusStrings
    )
    val algebraInts = new setsWithInvolution.Algebra[Int](
      StandardTermsAndOperators.~ := minusInts
    )
    algebraStrings.sanityTest
    algebraInts.sanityTest

    val morphism: String => Int = Map("+" -> 1, "-" -> -1)
    setsWithInvolution.isMorphism(algebraStrings, algebraInts, morphism) is true

    val notAMorphism: String => Int = Map("+" -> 1, "-" -> 1)
    setsWithInvolution.isMorphism(algebraStrings, algebraInts, notAMorphism) is false
  }

  test("Algebraic theories can validate morphisms preserving constants") {
    implicit val carrierInts: Set[Int] = Set[Int](0, 1, 2)
    val pointInts: Unit => Int = makeNullaryOperator(0)
    implicit val carrierStrings: Set[String] = Set[String]("samson", "delilah")
    val pointStrings: Unit => String = makeNullaryOperator("delilah")

    val pointedSets = AlgebraicTheory(o)()
    val algebraStrings = new pointedSets.Algebra[String](o := pointStrings)
    val algebraInts = new pointedSets.Algebra[Int](o := pointInts)

    val morphism: String => Int = Map("samson" -> 1, "delilah" -> 0)
    val notAMorphism: String => Int = Map("samson" -> 1, "delilah" -> 1)

    pointedSets.isMorphism(algebraStrings, algebraInts, morphism) is true
    pointedSets.isMorphism(algebraStrings, algebraInts, notAMorphism) is false
  }

  test("Algebraic theories can validate morphisms preserving binary operations") {
    implicit val carrierInts: Set[Int] = Set[Int](0, 1, 2, 3)
    val multiplication: ((Int, Int)) => Int = Function.tupled {
      (x, y) => (x + y) % 4
    }

    val magmas = AlgebraicTheory(*)()
    val algebra = new magmas.Algebra[Int](* := multiplication)

    val morphism: Int => Int = Map(0 -> 0, 1 -> 2, 2 -> 0, 3 -> 2)
    val notAMorphism: Int => Int = Map(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 0)

    magmas.isMorphism(algebra, algebra, morphism) is true
    magmas.isMorphism(algebra, algebra, notAMorphism) is false
  }

  test("Algebraic theories can validate morphisms preserving mixed operations with scalars") {
    val q = Symbol("q")
    implicit val scalars: Set[Symbol] = Set(q)
    val pointScalar: Unit => Symbol = makeNullaryOperator[Symbol](q)
    val multiplication: ((Boolean, Symbol)) => Boolean = Map(
        (true, q) -> false,
        (false, q) -> true
      )

    val weakActsOverAPointedMagma = AlgebraicTheoryWithScalars[Symbol](II := pointScalar)(II, **)()
    val algebra = new weakActsOverAPointedMagma.Algebra[Boolean](** := multiplication)
    algebra.sanityTest

    val morphism: Boolean => Boolean = Map(true -> false, false -> true)
    val notAMorphism: Boolean => Boolean = Map(true -> true, false -> true)

    weakActsOverAPointedMagma.isMorphism(algebra, algebra, morphism) is true
    weakActsOverAPointedMagma.isMorphism(algebra, algebra, notAMorphism) is false
  }

  test("Algebraic theories support named laws with error reporting when they fail") {
    implicit val carrier: Set[Int] = Set(1, -1)
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
    new setsWithInvolution.Algebra[Int](StandardTermsAndOperators.~ := minusGood).sanityTest
    intercept[IllegalArgumentException] {
      new setsWithInvolution.Algebra[Int](StandardTermsAndOperators.~ := minusBad).sanityTest
    }.getMessage.contains("involutive") is true
  }

  test("Algebraic theories can verify additional laws once an algebra is constructed") {
    implicit val carrier: Set[Int] = Set[Int](1, -1)
    val minus: Int => Int = Map(
      1 -> -1,
      -1 -> 1
    )

    val setsWithInvolution = AlgebraicTheory(~)(
      "involutive" law (~(~α) := α)
    )
    val algebra = new setsWithInvolution.Algebra[Int](StandardTermsAndOperators.~ := minus)
    algebra.sanityTest

    algebra.satisfies(
      ~α := α
    ) is false
    algebra.satisfies(
      ~α := ~(~(~α))
    ) is true
  }

  test("Algebraic theories can be extended by adding new constants/operations, and laws ") {
    val magmas = AlgebraicTheory(*)()
    val commutativeMagmasWith1 =
      magmas.extend(ι)(
        "commutative" law( α * β := β * α ),
        "unit" law( α * ι := α )
      )
    case class CommutativeMagmaWith1[T: Set](
      unit: NullaryOp[T],
      op: BinaryOp[T]
    ) extends commutativeMagmasWith1.Algebra[T](
      ι := unit,
      * := op
    )

    val commutativeOp: ((Boolean, Boolean)) => Boolean = Function.tupled { _ & _}
    val nonCommutativeOp: ((Boolean, Boolean)) => Boolean = Function.tupled{ _ & !_ }
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

  private val localGroups = AlgebraicTheory(ι, StandardTermsAndOperators.~, *)(
    "left unit" law { ι * α := α },
    "right unit" law { α * ι := α },
    "left inverse" law { (~α) * α := ι },
    "associative" law { (α * β) * γ := α * (β * γ) }
  )

  private class LocalGroup[G : Set](
   unit: NullaryOp[G],
   multiply: BinaryOp[G],
   inverse: UnaryOp[G]
  ) extends localGroups.Algebra[G](
    ι := unit,
    * := multiply,
    StandardTermsAndOperators.~ := inverse
  ) {
    // Formalism to handle direct products more elegantly with added sugar
    def x[H : Set](
      that: LocalGroup[H]
    ): LocalGroup[(G, H)] = {
      val product = (this: localGroups.Algebra[G]) x (that)
      new LocalGroup[(G, H)](
        product.operatorAssignments.lookup(ι).get,
        product.operatorAssignments.lookup(*).get,
        product.operatorAssignments.lookup(StandardTermsAndOperators.~).get
      )
    }
  }

//    [Int4] => Set[Int4]
  private def withIntsMod[R](
    modulus: Int
  )(
    block: [I] => Set[I] ?=> I =:= Int ?=> Int =:= I ?=> LocalGroup[I] => R
  ): R =
    maskSetDot[Int, R](
      dot = 0 until modulus toSet
    ) ( [I] => (_: Set[I]) ?=> (I_is_Int: I =:= Int) ?=>
      implicit val Int_is_I: Int =:= I = I_is_Int.flip
      val group =
        new LocalGroup[I](
          makeNullaryOperator[I](0),
          Function.tupled[I, I, I] {
            (x, y) => (x + y) % modulus
          },
          { (i: I) => (modulus - i) % modulus }
        )

      block[I](group)
    )

  test("Algebraic theories support binary multiplication of their algebras") {
    withIntsMod[Unit](6) {
      [Int6] => (_: Set[Int6]) ?=> (_: (Int6 =:= Int)) ?=> (_: (Int =:= Int6)) ?=> (group6: LocalGroup[Int6]) => {
        ()
      }
    }

    withIntsMod[Unit](2) {
    [Int2] => (_: Set[Int2]) ?=> (_: Int2 =:= Int) ?=> (_: Int =:= Int2) ?=> (group2: LocalGroup[Int2]) =>
      withIntsMod[Unit](3) {
        [Int3] => (_: Set[Int3]) ?=> (_: Int3 =:= Int) ?=> (_: Int =:= Int3) ?=> (group3: LocalGroup[Int3]) =>
          withIntsMod[Unit](6) {
            [Int6] => (_: Set[Int6]) ?=> (_: Int6 =:= Int) ?=> (_: Int =:= Int6) ?=> (group6: LocalGroup[Int6]) => {
              group2.sanityTest
              group3.sanityTest
              group6.sanityTest

              val group2x3: LocalGroup[(Int2, Int3)] = group2 x group3
              group2x3.sanityTest

              localGroups.isMorphism(
                group2x3,
                group2,
                { _ => 0 }
              ) is true

              localGroups.isMorphism(
                group2x3,
                group2,
                { _ => 1 }
              ) is false

              localGroups.isMorphism(
                group2x3,
                group2,
                π0[Int2, Int3]
              ) is true

              localGroups.isMorphism(
                group2x3,
                group3,
                π1[Int2, Int3]
              ) is true

              val chineseRemainder: Int6 => (Int2, Int3) =
                { i => (i % 2, i % 3) }
              chineseRemainder.isIsoPlaceholderTrue is true

              localGroups.isMorphism(
                group6,
                group2x3,
                chineseRemainder
              ) is true

              val notChineseRemainder: Int6 => (Int2, Int3) =
                { i => ((i + 1) % 2, (i + 2) % 3) }
              notChineseRemainder.isIsoPlaceholderTrue is true

              localGroups.isMorphism(
                group6,
                group2x3,
                notChineseRemainder
              ) is false

              ()
            }
          }
      }
    }
  }

  /*
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

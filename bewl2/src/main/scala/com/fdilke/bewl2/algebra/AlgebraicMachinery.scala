package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.Mappable
import com.fdilke.utility.Shortcuts.*

import scala.annotation.targetName
import scala.language.{dynamics, postfixOps}
import Mappable._

sealed trait AlgebraicSort
class Principal extends AlgebraicSort
class Scalar extends AlgebraicSort

trait AlgebraicMachinery[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  →[_, _]
] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, →] =>

  type NullaryOp[X] = UNIT ~> X
  type UnaryOp[X] = X ~> X
  type BinaryOp[X] = BiArrow[X, X, X]
  type RightScalarBinaryOp[X, S] = BiArrow[X, S, X]

  case class Law(
    left: Term[Principal],
    right: Term[Principal],
    name: Option[String] = None
  ) {
    def fails =
      bail(
        name.getOrElse("Unnamed") + " law failed"
      )

    val freeVariables =
      (left.freeVariables ++
        right.freeVariables) distinct

    def named(name: String) =
      Law(left, right, Some(name))
  }

  object NamedLaws {
    implicit class NamedLaw(
         name: String
     ) {
      def law(unnamedLaw: Law) =
        unnamedLaw.named(name)
    }
  }

  sealed trait Term[
    X <: AlgebraicSort
  ](
    val freeVariables: Seq[VariableTerm[_ <: AlgebraicSort]]
  ) extends Dynamic {
    def applyDynamic(
      name: String
    )(
      other: Term[X]
    ) =
      BinaryOpTerm(
        this,
        StandardTermsAndOperators.binaryOpFrom(name),
        other
      )

    def +(other: Term[X]) =
      applyDynamic("+")(other)

    def *(other: Term[X]) =
      applyDynamic("*")(other)

    def →(other: Term[X]) =
      applyDynamic("→")(other)

    def **(
      other: Term[Scalar]
    )(
      implicit eq: =:=[X, Principal]
    ) =
      BinaryRightScalarOpTerm(
        this.asInstanceOf[Term[Principal]], // cast justified by =:=
        StandardTermsAndOperators.**,
        other
      )

    def ***(
      other: Term[Scalar]
    )(
      implicit eq: =:=[X, Scalar]
    ) =
      BinaryScalarOpTerm(
        this.asInstanceOf[Term[Scalar]], // cast justified by =:=
        StandardTermsAndOperators.***,
        other
      )

    def unary_~ : Term[X] =
      UnaryOpTerm(StandardTermsAndOperators.~, this)

    def :=(
      that: Term[Principal]
    )(
      implicit eq: =:=[X, Principal]
    ) =
      Law(
        this.asInstanceOf[Term[Principal]], // cast justified by =:=
        that
      )
  }

  case class Operator(
    name: String,
    arity: Int
  )

  case class VariableTerm[
    S <: AlgebraicSort
  ](
   symbol: String,
   isScalar: Boolean
  ) extends Term[S](Nil) {
    override val freeVariables: Seq[VariableTerm[_ <: AlgebraicSort]] = Seq(this)
  }

  object PrincipalTerm {
    def apply(
      symbol: String
    ) =
      VariableTerm[Principal](
        symbol,
        isScalar = false
      )
  }

  object ScalarTerm {
    def apply(
      symbol: String
    ) =
      VariableTerm[Scalar](
        symbol,
        isScalar = true
      )
  }

  case class BinaryOpTerm[S <: AlgebraicSort](
    left: Term[S],
    op: AbstractBinaryOp,
    right: Term[S]
  ) extends Term[S](
    (left.freeVariables ++ right.freeVariables).distinct
  )

  case class BinaryRightScalarOpTerm(
    left: Term[Principal],
    op: AbstractRightScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Principal](
    (left.freeVariables ++ right.freeVariables).distinct
  )

  case class BinaryScalarOpTerm(
    left: Term[Scalar],
    op: AbstractScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Scalar](
    (left.freeVariables ++ right.freeVariables).distinct
  )

  case class UnaryOpTerm[S <: AlgebraicSort](
    op: AbstractUnaryOp,
    innerTerm: Term[S]
  ) extends Term[S](
    innerTerm.freeVariables
  )

  class ConstantOperator[
    X <: AlgebraicSort
  ](
    name: String
  ) extends Operator(name, arity = 0)
    with Term[X](Nil)

  class PrincipalConstant(
    name: String
  ) extends ConstantOperator[Principal](
    name
  ) {
    def :=[
      S : DOT,
      T : DOT
    ](
      nullaryOp: NullaryOp[T]
    ) =
      new OperatorAssignment[T, S](operator = this) {
        override def lookupPrincipalConstant: Option[NullaryOp[T]] =
          Some(nullaryOp)
      }
  }

  class ScalarConstant(
    name: String
  ) extends ConstantOperator[Scalar](
    name
  ) {
    def :=[
      T : DOT
    ](
       nullaryOp: NullaryOp[T]
    ): OperatorPreassignment[T] =
      new OperatorPreassignment[T](op = ScalarConstant.this) {
        override def lookupScalarConstant: Option[NullaryOp[T]] =
          Some(nullaryOp)
      }
  }

  class OperatorPreassignment[S: DOT](
    op: Operator
  ) extends OperatorAssignment[UNIT, S](op)

  abstract case class OperatorAssignment[
    T : DOT,
    S : DOT
  ](
     operator: Operator
   ) {
    // TODO: refactor this to be table-driven
    def lookupPrincipalConstant: Option[NullaryOp[T]] = None

    def lookupUnaryOp: Option[UnaryOp[T]] = None

    def lookupBinaryOp: Option[BinaryOp[T]] = None

    def lookupScalarConstant: Option[NullaryOp[S]] = None

    def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[T, S]] = None

    def lookupScalarBinaryOp: Option[BinaryOp[S]] = None

    final def sanityTest: Unit = {
      lookupPrincipalConstant foreach {
        _.sanityTest
      }
      lookupUnaryOp foreach {
        _.sanityTest
      }
      lookupBinaryOp foreach {
        _.sanityTest
      }
      lookupScalarConstant foreach {
        _.sanityTest
      }
      lookupRightScalarBinaryOp foreach {
        _.sanityTest
      }
      lookupScalarBinaryOp foreach {
        _.sanityTest
      }
    }
  }

  case class OperatorAssignments[
    T : DOT,
    S : DOT
  ](
     assignments: Seq[OperatorAssignment[T, S]]
  ) {
    private def doLookup[OP](
      op: Operator
    )(
      handleAssignment: OperatorAssignment[T, S] => Option[OP]
    ): Option[OP] =
      assignments find {
        _.operator == op
      } flatMap
        handleAssignment

    def lookup(
      principalConstant: PrincipalConstant
    ): Option[NullaryOp[T]] =
      doLookup(principalConstant) {
        _.lookupPrincipalConstant
      }

    def lookup(
      scalarConstant: ScalarConstant
    ): Option[NullaryOp[S]] =
      doLookup(scalarConstant) {
        _.lookupScalarConstant
      }

    def lookup(
      unaryOp: AbstractUnaryOp
    ): Option[UnaryOp[T]] =
      doLookup(unaryOp) {
        _.lookupUnaryOp
      }

    def lookup(
      binaryOp: AbstractBinaryOp
    ): Option[BinaryOp[T]] =
      doLookup(binaryOp) {
        _.lookupBinaryOp
      }

    def lookup(
      op: AbstractRightScalarBinaryOp
    ): Option[RightScalarBinaryOp[T, S]] =
      doLookup(op) {
        _.lookupRightScalarBinaryOp
      }

    def lookup(
      op: AbstractScalarBinaryOp
    ): Option[BinaryOp[S]] =
      doLookup(op) {
        _.lookupScalarBinaryOp
      }

    def hasPrecisely(
      operators: Seq[Operator]
    ): Boolean =
      assignments.map {
        _.operator
      }.toSet ==
        operators.toSet

    def crossedWith[U : DOT](
     that: OperatorAssignments[U, S]
//     productCarrier: BiProduct[T, U],
//     scalars: DOT[S]
   ): Seq[OperatorAssignment[(T, U), S]] =
      assignments map { assignment =>
        import assignment.operator
        that.doLookup(operator) { thatAssignment =>
          Some(
            new OperatorAssignment[(T, U), S](operator) {
              override def lookupUnaryOp: Option[UnaryOp[(T, U)]] =
                for {
                  op: UnaryOp[T] <- assignment.lookupUnaryOp
                  thatOp: UnaryOp[U] <- thatAssignment.lookupUnaryOp
                } yield { (tu: CTXT[(T, U)]) =>
                    productMagic[T, U](
                      op(tu.map(_._1)),
                      thatOp(tu.map(_._2))
                    )
                }

              override def lookupPrincipalConstant: Option[NullaryOp[(T, U)]] =
                for {
                  op <- assignment.lookupPrincipalConstant
                  thatOp <- thatAssignment.lookupPrincipalConstant
                } yield { (cu: CTXT[UNIT]) =>
                  productMagic(
                    op(cu),
                    thatOp(cu)
                  )
                }

              override def lookupBinaryOp: Option[BinaryOp[(T, U)]] =
                for {
                  op <- assignment.lookupBinaryOp
                  thatOp <- thatAssignment.lookupBinaryOp
                } yield { (pair: CTXT[((T, U), (T, U))]) =>
                  productMagic(
                    op(pair.map { tutu => tutu._1._1 -> tutu._2._1 }),
                    thatOp(pair.map { tutu => tutu._1._2 -> tutu._2._2 }),
                  )
                }

              override def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[(T, U), S]] =
                for {
                  op <- assignment.lookupRightScalarBinaryOp
                  thatOp <- thatAssignment.lookupRightScalarBinaryOp
                } yield { (pair: CTXT[((T, U), S)]) =>
                  productMagic(
                    op(pair.map { tu_s => tu_s._1._1 -> tu_s._2 }),
                    thatOp(pair.map { tu_s => tu_s._1._2 -> tu_s._2 }),
                  )
                }

//                  (productCarrier x scalars).biArrow(productCarrier) { (tu, s) =>
//                  val t = productCarrier.π0(tu)
//                  val u = productCarrier.π1(tu)
//                  productCarrier.pair(
//                    op(t, s),
//                    thatOp(u, s)
//                  )
//                }

              override def lookupScalarConstant: Option[NullaryOp[S]] =
                throw new IllegalArgumentException(
                  "algebra multiplication should not override scalar constant operator " + operator
                )

              override def lookupScalarBinaryOp: Option[BinaryOp[S]] =
                for {
                  op <- assignment.lookupScalarBinaryOp
                  thatOp <- thatAssignment.lookupScalarBinaryOp
                } yield
                  if (op == thatOp)
                    op
                  else
                    throw new IllegalArgumentException(
                      "algebra multiplication failed: inconsistent binary ops for operator " + operator
                    )
            }
          )
        } getOrElse {
          throw new IllegalArgumentException(
            "algebra multiplication failed: can't match operator " + operator
          )
        }
      }
  }

  class AbstractBinaryOp(
    name: String
  ) extends Operator(name, arity =2) {
    @targetName("definitionally")
    def :=[S : DOT, T : DOT](
      binaryOp: BinaryOp[T]
    ): OperatorAssignment[T, S] =
      new OperatorAssignment[T, S](operator = this) {
        override def lookupBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractRightScalarBinaryOp(
    name: String
  ) extends Operator(name, arity = 2) {
    @targetName("definitionally")
    def :=[T : DOT, S : DOT](
      binaryOp: RightScalarBinaryOp[T, S]
    ): OperatorAssignment[T, S] =
      new OperatorAssignment[T, S](this) {
        override def lookupRightScalarBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractScalarBinaryOp(
    name: String
  ) extends Operator(name, 2) {
    @targetName("definitionally")
    def :=[S : DOT, T : DOT](binaryOp: BinaryOp[S]) =
      new OperatorAssignment[T, S](this) {
        override def lookupScalarBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractUnaryOp(
    name: String
  ) extends Operator(name, arity = 1) {
    @targetName("definitionally")
    def :=[S : DOT, T : DOT](unaryOp: UnaryOp[T]): OperatorAssignment[T, S] =
      new OperatorAssignment[T, S](this) {
        override def lookupUnaryOp =
          Some(unaryOp)
      }
  }

  object StandardTermsAndOperators {
    val o = new PrincipalConstant("o")
    val ι = new PrincipalConstant("ι")
    val ⊥ = new PrincipalConstant("⊥")
    val ⊤ = new PrincipalConstant("⊤")
    val II = new ScalarConstant("II")

    val α = PrincipalTerm("α")
    val β = PrincipalTerm("β")
    val γ = PrincipalTerm("γ")
    val Φ = ScalarTerm("Φ")
    val Ψ = ScalarTerm("Ψ")

    val ~ = new AbstractUnaryOp("~")

    val * = new AbstractBinaryOp("*")
    val + = new AbstractBinaryOp("+")
    val ∧ = new AbstractBinaryOp("∧")
    val ∨ = new AbstractBinaryOp("∨")
    val → = new AbstractBinaryOp("→")

    val ** = new AbstractRightScalarBinaryOp("**")
    val *** = new AbstractScalarBinaryOp("***")

    private val binaryOperators =
      Map[String, AbstractBinaryOp](
        "*" -> *,
        "+" -> +,
        "∧" -> ∧,
        "∨" -> ∨,
        "→" -> →
      )

    def binaryOpFrom(name: String) =
      binaryOperators.getOrElse(
        name,
        bail(
          "Unknown binary operator: " + name
        )
      )
  }

  class AlgebraicTheory[
    S : DOT
  ](
     preassignments: OperatorPreassignment[S]*
   )(
     operators: Operator*
   )(
     laws: Law*
   ) {
    def extend(moreOperators: Operator*)(moreLaws: Law*) =
      new AlgebraicTheory[S]( // (scalars)
        preassignments: _*
      )(
        operators ++ moreOperators: _*
      )(
        laws ++ moreLaws: _*
      )

    def isMorphism[A : DOT, B : DOT](
      sourceAlgebra: Algebra[A],
      targetAlgebra: Algebra[B],
      arrow: A ~> B
    ): Boolean =
      operators forall {
        case op: ScalarConstant =>
          true

        case op: PrincipalConstant => (
          for {
            srcConstant <- sourceAlgebra.operatorAssignments.lookup(op)
            tgtConstant <- targetAlgebra.operatorAssignments.lookup(op)
          } yield (arrow o srcConstant) =!= tgtConstant
          ) getOrElse bail(
          "Not found in source algebra: " + op.name
        )

        case op: AbstractUnaryOp =>             (
          for {
            srcOp <- sourceAlgebra.operatorAssignments.lookup(op)
            tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
          } yield (arrow o srcOp) =!= (tgtOp o arrow)
          ) getOrElse bail(
          "Not found in source algebra: " + op.name
        )

        case op: AbstractBinaryOp =>
          (
            for {
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op)
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            } yield {
              (arrow o srcOp) =!= { (aa: CTXT[(A, A)]) =>
                tgtOp(
                  productMagic[B, B](
                    arrow(aa.map{_._1}),
                    arrow(aa.map{_._2})
                  )
                )
              }
            }
          ) getOrElse bail(
            "Not found in source algebra: " + op.name
          )

        case op: AbstractRightScalarBinaryOp =>
          (
            for {
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op)
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            } yield {
              (arrow o srcOp) =!= { (as: CTXT[(A, S)]) =>
                tgtOp(
                  productMagic[B, S](
                    arrow(as.map {_._1}),
                    as.map{_._2}
                  )
                )
              }
            }
          ) getOrElse bail(
            "Not found in source algebra: " + op.name
          )

        case op: AbstractScalarBinaryOp =>
          true // free pass, no need to verify these on carrier

        case op =>
          bail(
            s"Unknown type of operator, can't verify: ${op.name}"
          )
      }

    class Algebra[T : DOT](
     private val assignments: OperatorAssignment[T, S]*
    ) { algebra =>
      val operatorAssignments: OperatorAssignments[T, S] =
        OperatorAssignments(
          (preassignments map {
            _.asInstanceOf[OperatorAssignment[T, S]]
          }) ++ assignments
        )

      def x[U : DOT](
       that: Algebra[U]
      ): Algebra[(T, U)] = {
        new Algebra[(T, U)](
          OperatorAssignments(
            assignments
          ).crossedWith(
            OperatorAssignments(that.assignments)
          ): _*
        )
      }

      object EvaluationContext {
        def apply(
          variables: Seq[VariableTerm[_ <: AlgebraicSort]]
        ): EvaluationContext[_] =
          variables.foldRight[EvaluationContext[_]](
            SimpleEvaluationContext
          )(
            addVariableToContext
          )

        private def addVariableToContext(
          variable: VariableTerm[_ <: AlgebraicSort],
          context: EvaluationContext[_]
        ): EvaluationContext[_] =
          if (variable.isScalar) then // TODO: fold into variable
            context.spawnCompound[S](variable.symbol)
          else
            context.spawnCompound[T](variable.symbol)
      }

      trait EvaluationContext[ROOT: DOT] {

        def evaluate(
          term: Term[Principal]
        ): ROOT ~> T

        def evaluateScalar(
          term: Term[Scalar]
        ): ROOT ~> S

        protected def evaluateScalarConstant(
          term: ScalarConstant
        ): ROOT ~> S =
          operatorAssignments.lookup(term) map { constant =>
            constant o toUnit[ROOT]
          } getOrElse bail(
            "Unknown constant in expression: " + term.name
          )

        protected def evaluatePrincipalConstant(
          term: PrincipalConstant
        ): ROOT ~> T =
          operatorAssignments.lookup(term) map { constant =>
            constant o toUnit[ROOT]
          } getOrElse bail(
            "Unknown constant in expression: " + term.name
          )

        protected def evaluateBinaryScalarOpTerm(
          term: BinaryScalarOpTerm
        ): ROOT ~> S =
          operatorAssignments.lookup(term.op) map { op => // root(scalars)
            { (r: CTXT[ROOT]) =>
              op(
                productMagic[S, S](
                  evaluateScalar(term.left)(r),
                  evaluateScalar(term.right)(r)
                )
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateBinaryOpTerm(
          term: BinaryOpTerm[Principal]
        ): ROOT ~> T =
          operatorAssignments.lookup(term.op) map { op => // root(carrier)
            { (r: CTXT[ROOT]) =>
              op(
                productMagic[T, T](
                  evaluate(term.left)(r),
                  evaluate(term.right)(r)
                )
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateBinaryRightScalarOpTerm(
          term: BinaryRightScalarOpTerm
        ): ROOT ~> T =
          operatorAssignments.lookup(term.op) map { op => // root(carrier)
            { (r: CTXT[ROOT]) =>
              op(
                productMagic[T, S](
                  evaluate(term.left)(r),
                  evaluateScalar(term.right)(r)
                )
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateUnaryOpTerm(
          term: UnaryOpTerm[Principal]
        ): ROOT ~> T =
          operatorAssignments.lookup(term.op) map { op => // root(carrier)
            { (r: CTXT[ROOT]) =>
              op(
                evaluate(term.innerTerm)(r)
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        def doesSatisfy(
          law: Law
        ): Boolean =
            evaluate(law.left) =!=
              evaluate(law.right)

        def spawnCompound[U: DOT](
          symbol: String
        ): EvaluationContext[(U, ROOT)] =
          new CompoundEvaluationContext[U, ROOT](
            symbol,
            this
          )
      }

      object SimpleEvaluationContext extends EvaluationContext[UNIT] {

        override def evaluate(
          term: Term[Principal]
        ): UNIT ~> T =
          term match {
            case term: PrincipalConstant =>
              evaluatePrincipalConstant(term)

            case _ =>
              bail(
                "No variables available for principal term: " + term
              )
          }

        override def evaluateScalar(
          term: Term[Scalar]
        ): UNIT ~> S =
          term match {
            case term: ScalarConstant =>
              evaluateScalarConstant(term)

            case term: BinaryScalarOpTerm =>
              evaluateBinaryScalarOpTerm(term)

            case _ =>
              bail(
                "No variables available for scalar term: " + term
              )
          }
      }

      class CompoundEvaluationContext[HEAD : DOT, TAIL:DOT](
        name: String,
        val tail: EvaluationContext[TAIL]
      ) extends EvaluationContext[(HEAD, TAIL)] {

        override def evaluate(
          term: Term[Principal]
        ): (HEAD, TAIL) ~> T =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              π0[HEAD, TAIL].asInstanceOf[(HEAD, TAIL) ~> T]

            case term: BinaryOpTerm[Principal] =>
              evaluateBinaryOpTerm(term)

            case term: BinaryRightScalarOpTerm =>
              evaluateBinaryRightScalarOpTerm(term)

            case term: UnaryOpTerm[Principal] =>
              evaluateUnaryOpTerm(term)

            case _ =>
              tail.evaluate(term) o π1[HEAD, TAIL]
          }

        override def evaluateScalar(
          term: Term[Scalar]
        ): (HEAD, TAIL) ~> S =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              π0[HEAD, TAIL].asInstanceOf[(HEAD, TAIL) ~> S]

            case term: BinaryScalarOpTerm =>
              evaluateBinaryScalarOpTerm(term)

            case _ =>
              tail.evaluateScalar(term) o π1[HEAD, TAIL]
          }
      }

      def satisfies(law: Law): Boolean =
        EvaluationContext(
          law.freeVariables
        ).doesSatisfy(law)

      def sanityTest: Unit = {
        if (!operatorAssignments.hasPrecisely(operators))
          bail("Assignments do not match signature of theory")

        operatorAssignments.assignments foreach {
          _ sanityTest
        }

        laws foreach { law =>
          if (!satisfies(law))
            law.fails
        }
      }
    }
  }

  object AlgebraicTheory:
    def apply(operators: Operator*)(laws: Law*) =
      new AlgebraicTheory[UNIT]()(operators: _*)(laws: _*)

  object AlgebraicTheoryWithScalars:
    def apply[S : DOT](
       preassignments: OperatorPreassignment[S]*
     )(
       operators: Operator*
     )(
       laws: Law*
     ) =
      new AlgebraicTheory[S](preassignments: _*)(operators: _*)(laws: _*)

  type Algebra = AlgebraicTheory[_]#Algebra[_]
}


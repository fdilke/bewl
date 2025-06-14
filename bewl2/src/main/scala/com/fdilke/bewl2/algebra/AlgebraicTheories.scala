package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.topos.ProductMappable
import com.fdilke.bewl2.topos.Topos
import com.fdilke.utility.Shortcuts.*

import scala.language.{dynamics, postfixOps}
import ProductMappable._

trait AlgebraicTheories[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  type NullaryOp[X] = UNIT ~> X
  type UnaryOp[X] = X ~> X
  type BinaryOp[X] = BiArrow[X, X, X]
  type RightScalarBinaryOp[X, S] = BiArrow[X, S, X]

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
    val ! = new AbstractUnaryOp("!")

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
        bail("Unknown binary operator: " + name)
      )
  }

  import StandardTermsAndOperators._
  import StandardTermsAndOperators.!
  import StandardTermsAndOperators.~
  import StandardTermsAndOperators.{ ** => `_**` }
  import StandardTermsAndOperators.{ *** => `_***` }

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

  extension(name: String)
    infix def law(unnamedLaw: Law) =
      unnamedLaw.named(name)

  sealed trait Term[
    X <: AlgebraicSort
  ](
     val initFreeVariables: Seq[VariableTerm[? <: AlgebraicSort]]
   ) extends Dynamic {
    def freeVariables: Seq[VariableTerm[? <: AlgebraicSort]] =
      initFreeVariables
      
    def applyDynamic(
      name: String
    )(
      other: Term[X]
    ) =
      BinaryOpTerm(
        this,
        binaryOpFrom(name),
        other
      )

    def +(other: Term[X]) =
      applyDynamic("+")(other)

    def *(other: Term[X]) =
      applyDynamic("*")(other)

    def →(other: Term[X]) =
      applyDynamic("→")(other)

    def **(other: Term[Scalar])(
      implicit eq: X =:= Principal
    ) =
      BinaryRightScalarOpTerm(
        this.asInstanceOf[Term[Principal]], // cast justified by =:=
        `_**`,
        other
      )

    def ***(other: Term[Scalar])(
      implicit eq: X =:= Scalar
    ) =
      BinaryScalarOpTerm(
        this.asInstanceOf[Term[Scalar]], // cast justified by =:=
        `_***`,
        other
      )

    def unary_~ : Term[X] =
      UnaryOpTerm(~, this)

    def unary_! : Term[X] =
      UnaryOpTerm(!, this)

    def :=(that: Term[Principal])(
      implicit eq: X =:= Principal
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
    override def freeVariables: Seq[VariableTerm[? <: AlgebraicSort]] =
      Seq(this)

    def addToContext[S : Dot, T : Dot](
      algebra: AlgebraicTheory[S]#Algebra[T]
    )(
      context: algebra.EvaluationContext[?]
    ): algebra.EvaluationContext[?] =
      if (isScalar) then
        context.spawnCompound[S](symbol)
      else
        context.spawnCompound[T](symbol)
  }

  object PrincipalTerm:
    def apply(symbol: String) =
      VariableTerm[Principal](
        symbol,
        isScalar = false
      )

  object ScalarTerm:
    def apply(symbol: String) =
      VariableTerm[Scalar](
        symbol,
        isScalar = true
      )

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
      S : Dot,
      T : Dot
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
      T : Dot
    ](
       nullaryOp: NullaryOp[T]
     ): OperatorPreassignment[T] =
      new OperatorPreassignment[T](op = ScalarConstant.this) {
        override def lookupScalarConstant: Option[NullaryOp[T]] =
          Some(nullaryOp)
      }
  }

  class OperatorPreassignment[S: Dot](
    op: Operator
  ) extends OperatorAssignment[UNIT, S](op)

  abstract case class OperatorAssignment[
    T : Dot,
    S : Dot
  ](
     operator: Operator
   ) {
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
    T : Dot,
    S : Dot
  ](
     assignments: Seq[OperatorAssignment[T, S]]
   ):
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
      doLookup(principalConstant):
        _.lookupPrincipalConstant

    def lookup(
      scalarConstant: ScalarConstant
    ): Option[NullaryOp[S]] =
      doLookup(scalarConstant):
        _.lookupScalarConstant

    def lookup(
      unaryOp: AbstractUnaryOp
    ): Option[UnaryOp[T]] =
      doLookup(unaryOp):
        _.lookupUnaryOp

    def lookup(
      binaryOp: AbstractBinaryOp
    ): Option[BinaryOp[T]] =
      doLookup(binaryOp):
        _.lookupBinaryOp

    def lookup(
      op: AbstractRightScalarBinaryOp
    ): Option[RightScalarBinaryOp[T, S]] =
      doLookup(op):
        _.lookupRightScalarBinaryOp

    def lookup(
      op: AbstractScalarBinaryOp
    ): Option[BinaryOp[S]] =
      doLookup(op):
        _.lookupScalarBinaryOp

    def hasPrecisely(
      operators: Seq[Operator]
    ): Boolean =
      assignments.map:
        _.operator
      .toSet ==
        operators.toSet

    def crossedWith[U : Dot](
      that: OperatorAssignments[U, S]
    ): Seq[OperatorAssignment[(T, U), S]] =
      assignments map: assignment =>
        import assignment.operator
        that.doLookup(operator): thatAssignment =>
          Some(
            new OperatorAssignment[(T, U), S](operator) {
              override def lookupUnaryOp: Option[UnaryOp[(T, U)]] =
                for {
                  op: UnaryOp[T] <- assignment.lookupUnaryOp
                  thatOp: UnaryOp[U] <- thatAssignment.lookupUnaryOp
                } yield { case t ⊕ u =>
                  op(t) ⊕ thatOp(u)
                }

              override def lookupPrincipalConstant: Option[NullaryOp[(T, U)]] =
                for {
                  op <- assignment.lookupPrincipalConstant
                  thatOp <- thatAssignment.lookupPrincipalConstant
                } yield { (cu: CTXT[UNIT]) =>
                  op(cu) ⊕
                  thatOp(cu)
                }

              override def lookupBinaryOp: Option[BinaryOp[(T, U)]] =
                for {
                  op <- assignment.lookupBinaryOp
                  thatOp <- thatAssignment.lookupBinaryOp
                } yield { case (t ⊕ u) ⊕ (t2 ⊕ u2) =>
                    op(t ⊕ t2) ⊕ thatOp(u ⊕ u2)
                }

              override def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[(T, U), S]] =
                for {
                  op <- assignment.lookupRightScalarBinaryOp
                  thatOp <- thatAssignment.lookupRightScalarBinaryOp
                } yield { case (t ⊕ u) ⊕ s =>
                  op(t ⊕ s) ⊕ thatOp(u ⊕ s)
                }

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
        .getOrElse:
          throw IllegalArgumentException:
            s"algebra multiplication failed: can't match operator $operator"

  class AbstractBinaryOp(
    name: String
  ) extends Operator(name, arity = 2):
    def :=[S : Dot, T : Dot](
      binaryOp: BinaryOp[T]
    ): OperatorAssignment[T, S] =
      new OperatorAssignment[T, S](operator = this):
        override def lookupBinaryOp =
          Some(binaryOp)

  class AbstractRightScalarBinaryOp(
    name: String
  ) extends Operator(name, arity = 2):
    def :=[T : Dot, S : Dot](
      binaryOp: RightScalarBinaryOp[T, S]
    ): OperatorAssignment[T, S] =
      new OperatorAssignment[T, S](this):
        override def lookupRightScalarBinaryOp =
          Some(binaryOp)

  class AbstractScalarBinaryOp(
    name: String
  ) extends Operator(name, 2):
    def :=[S : Dot](binaryOp: BinaryOp[S]) =
      new OperatorPreassignment[S](this):
        override def lookupScalarBinaryOp =
          Some(binaryOp)

  class AbstractUnaryOp(
    name: String
  ) extends Operator(name, arity = 1):
    def :=[S : Dot, T : Dot](
      unaryOp: UnaryOp[T]
    ): OperatorAssignment[T, S] =
      new OperatorAssignment[T, S](this):
        override def lookupUnaryOp =
          Some(unaryOp)

  class AlgebraicTheory[
    S : Dot
  ](
     preassignments: OperatorPreassignment[S]*
   )(
     operators: Operator*
   )(
     laws: Law*
   ) {
    def extend(moreOperators: Operator*)(moreLaws: Law*) =
      new AlgebraicTheory[S]( // (scalars)
        preassignments*
      )(
        operators ++ moreOperators*
      )(
        laws ++ moreLaws*
      )

    def isMorphism[A : Dot, B : Dot](
      arrow: A ~> B
    )(implicit
      sourceAlgebra: Algebra[A],
      targetAlgebra: Algebra[B]
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
              (arrow o srcOp) =!= { case a ⊕ b =>
                tgtOp(
                  arrow(a) ⊕ arrow(b)
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
              (arrow o srcOp) =!= { case a ⊕ s =>
                tgtOp(
                  arrow(a) ⊕ s
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

    class Algebra[T : Dot](
      private val assignments: OperatorAssignment[T, S]*
    ) { algebra =>
      val operatorAssignments: OperatorAssignments[T, S] =
        OperatorAssignments(
          (preassignments map {
            _.asInstanceOf[OperatorAssignment[T, S]]
          }) ++ assignments
        )

      infix def x[U : Dot](
        that: Algebra[U]
      ): Algebra[(T, U)] = {
        new Algebra[(T, U)](
          OperatorAssignments(
            assignments
          ).crossedWith(
            OperatorAssignments(that.assignments)
          )*
        )
      }

      object EvaluationContext {
        def apply(
          variables: Seq[VariableTerm[? <: AlgebraicSort]]
        ): EvaluationContext[?] =
          variables.foldRight[EvaluationContext[?]](
            SimpleEvaluationContext
          ) {
            _.addToContext(algebra = Algebra.this)(_)
          }
      }

      trait EvaluationContext[ROOT: Dot] {

        def evaluatePrincipal(
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
          operatorAssignments.lookup(term.op) map { op => (r: CTXT[ROOT]) =>
            op(
              evaluateScalar(term.left)(r) ⊕
              evaluateScalar(term.right)(r)
            )
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateBinaryOpTerm(
          term: BinaryOpTerm[Principal]
        ): ROOT ~> T =
          operatorAssignments.lookup(term.op) map { op => (r: CTXT[ROOT]) =>
            op(
              evaluatePrincipal(term.left)(r) ⊕
              evaluatePrincipal(term.right)(r)
            )
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateBinaryRightScalarOpTerm(
          term: BinaryRightScalarOpTerm
        ): ROOT ~> T =
          operatorAssignments.lookup(term.op) map { op => (r: CTXT[ROOT]) =>
            op(
              evaluatePrincipal(term.left)(r) ⊕
              evaluateScalar(term.right)(r)
            )
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateUnaryOpTerm(
          term: UnaryOpTerm[Principal]
        ): ROOT ~> T =
          operatorAssignments.lookup(term.op) map { op => (r: CTXT[ROOT]) =>
              op(evaluatePrincipal(term.innerTerm)(r))
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        infix def doesSatisfy(
          law: Law
        ): Boolean =
          evaluatePrincipal(law.left) =!=
            evaluatePrincipal(law.right)

        def spawnCompound[U: Dot](
          symbol: String
        ): EvaluationContext[?] =
          new CompoundEvaluationContext[U, ROOT](
            symbol,
            this
          )
      }

      object SimpleEvaluationContext extends EvaluationContext[UNIT] {
        override def evaluatePrincipal(
          term: Term[Principal]
        ): NullaryOp[T] =
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
        ): NullaryOp[S] =
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

        override def spawnCompound[U: Dot](
          symbol: String
        ): EvaluationContext[?] =
          new UnaryEvaluationContext[U](
            symbol
          )
      }

      class UnaryEvaluationContext[U: Dot](
        name: String
      ) extends EvaluationContext[U] {

        override def evaluatePrincipal(
          term: Term[Principal]
        ): U ~> T =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              id[U].asInstanceOf[U ~> T]

            case term: BinaryOpTerm[Principal] =>
              evaluateBinaryOpTerm(term)

            case term: BinaryRightScalarOpTerm =>
              evaluateBinaryRightScalarOpTerm(term)

            case term: UnaryOpTerm[Principal] =>
              evaluateUnaryOpTerm(term)

            case _ =>
              SimpleEvaluationContext.evaluatePrincipal(term) o toUnit[U]
          }

        override def evaluateScalar(
          term: Term[Scalar]
        ): U ~> S =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              id[U].asInstanceOf[U ~> S]

            case term: BinaryScalarOpTerm =>
              evaluateBinaryScalarOpTerm(term)

            case _ =>
              SimpleEvaluationContext.evaluateScalar(term) o toUnit[U]
          }
      }

      class CompoundEvaluationContext[HEAD : Dot, TAIL:Dot](
        name: String,
        val tail: EvaluationContext[TAIL]
      ) extends EvaluationContext[(HEAD, TAIL)] {

        override def evaluatePrincipal(
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
              tail.evaluatePrincipal(term) o π1[HEAD, TAIL]
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
        ) doesSatisfy law

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
    import topos.given
    def apply(operators: Operator*)(laws: Law*) =
      new AlgebraicTheory[UNIT]()(operators*)(laws*)

  object AlgebraicTheoryWithScalars:
    def apply[S : Dot](
      preassignments: OperatorPreassignment[S]*
    )(
      operators: Operator*
    )(
      laws: Law*
    ) =
      new AlgebraicTheory[S](preassignments*)(operators*)(laws*)

  type Variety = AlgebraicTheory[UNIT]
}



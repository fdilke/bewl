package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos.BaseTopos

import scala.language.{dynamics, implicitConversions, postfixOps, existentials}
import com.fdilke.bewl.helper.Shortcuts.bail

sealed trait AlgebraicSort
class Principal extends AlgebraicSort
class Scalar extends AlgebraicSort

trait AlgebraicMachinery { topos: BaseTopos =>

  type NullaryOp[X <: ~] = UNIT > X
  type UnaryOp[X <: ~] = X > X
  type BinaryOp[X <: ~] = BiArrow[X, X, X]
  type RightScalarBinaryOp[X <: ~, S <: ~] = BiArrow[X, S, X]

  case class Law(
    left: Term[Principal],
    right: Term[Principal],
    name: Option[String] = None
  ) {
    def isSatisfiedIn(
      context: Algebra#EvaluationContext
    ) =
      context.evaluate(left) ==
        context.evaluate(right)

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

  sealed trait Term[X <: AlgebraicSort] extends Dynamic {
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

    def unary_- : Term[X] =
      UnaryOpTerm(StandardTermsAndOperators.-, this)

    val freeVariables : Seq[VariableTerm[_ <: AlgebraicSort]]

    def :=(
      that: Term[Principal]
    )(
      implicit eq: =:=[X, Principal]
    ) =
      Law(
        this.asInstanceOf[Term[Principal]],
        that
      ) // cast justified by =:=
  }

  case class Operator(name: String, arity: Int)

  case class VariableTerm[
    S <: AlgebraicSort
  ](
    symbol: String,
    isScalar : Boolean
  ) extends Term[S] { term =>
    override val freeVariables = Seq(term)
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
  ) extends Term[S] {
    override val freeVariables =
      (left.freeVariables ++ right.freeVariables).distinct
  }

  case class BinaryRightScalarOpTerm(
    left: Term[Principal],
    op: AbstractRightScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Principal] {
    override val freeVariables =
      (left.freeVariables ++ right.freeVariables).distinct
  }

  case class BinaryScalarOpTerm(
    left: Term[Scalar],
    op: AbstractScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Scalar] {
    override val freeVariables =
      (left.freeVariables ++ right.freeVariables).distinct
  }

  case class UnaryOpTerm[S <: AlgebraicSort](
    op: AbstractUnaryOp,
    term: Term[S]
  ) extends Term[S] {
    override val freeVariables =
      term.freeVariables
  }

  class ConstantOperator[
    X <: AlgebraicSort
  ](
    name: String
  ) extends Operator(name, 0) with Term[X] {
    override val freeVariables =
      Nil
  }

  class PrincipalConstant(
    name: String
  ) extends ConstantOperator[Principal](
    name
  ) {
    def :=[
      T <: ~
    ](
      nullaryOp: NullaryOp[T]
    ) =
      new OperatorAssignment[T, ~](this) {
        override def lookupPrincipalConstant =
          Some(nullaryOp)
      }
  }

  class ScalarConstant(
    name: String
  ) extends ConstantOperator[Scalar](
    name
  ) {
    def :=[T <: ~](
      nullaryOp: NullaryOp[T]
    ) =
      new OperatorAssignment[~, T](this) {
        override def lookupScalarConstant =
          Some(nullaryOp)
      }
  }

  case class OperatorAssignment[
    T <: ~,
    S <: ~
  ](
    operator: Operator
  ) {
    def lookupPrincipalConstant: Option[NullaryOp[T]] = None
    def lookupScalarConstant: Option[NullaryOp[S]] = None
    def lookupUnaryOp: Option[UnaryOp[T]] = None
    def lookupBinaryOp: Option[BinaryOp[T]] = None
    def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[T, S]] = None
    def lookupScalarBinaryOp: Option[BinaryOp[S]] = None
  }

  case class OperatorAssignments[
    T <: ~,
    S <: ~
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
  }

  class AbstractBinaryOp(
    name: String
  ) extends Operator(name, 2) {
    def :=[T <: ~](
      binaryOp: BinaryOp[T]
    ) =
      new OperatorAssignment[T, ~](this){
        override def lookupBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractRightScalarBinaryOp(
    name: String
  ) extends Operator(name, 2) {
    def :=[T <: ~, S <: ~](
      binaryOp: RightScalarBinaryOp[T, S]
    ) =
      new OperatorAssignment[T, S](this){
        override def lookupRightScalarBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractScalarBinaryOp(
    name: String
  ) extends Operator(name, 2) {
    def :=[S <: ~, T <: ~](binaryOp: BinaryOp[S]) =
      new OperatorAssignment[T, S](this){
        override def lookupScalarBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractUnaryOp(
    name: String
  ) extends Operator(name, 1) {
    def :=[T <: ~](unaryOp: UnaryOp[T]) =
      new OperatorAssignment[T, ~](this){
        override def lookupUnaryOp =
          Some(unaryOp)
      }
  }

  object StandardTermsAndOperators {
    val o = new PrincipalConstant("o")
    val ι = new PrincipalConstant("ι")
    val II = new ScalarConstant("II")

    val α = PrincipalTerm("α")
    val β = PrincipalTerm("β")
    val γ = PrincipalTerm("γ")
    val Φ = ScalarTerm("Φ")
    val Ψ = ScalarTerm("Ψ")

    val - = new AbstractUnaryOp("-")

    val * = new AbstractBinaryOp("*")
    val + = new AbstractBinaryOp("+")

    val ** = new AbstractRightScalarBinaryOp("**")
    val *** = new AbstractScalarBinaryOp("***")

    private val binaryOperators =
      Map[String, AbstractBinaryOp](
        "*" -> *,
        "+" -> $plus
      )

    def binaryOpFrom(name: String) =
      binaryOperators.getOrElse(
        name,
        bail(
          "Unknown binary operator: " + name
        )
      )
  }

  class AlgebraicTheory[S <: ~](
      scalars: DOT[S]
   )(
      preassignments: OperatorAssignment[_ <: ~, _ <: ~]*
   )(
      operators: Operator*
   )(
      laws: Law*
   ){
    def isMorphism[A <: ~, B <: ~](
      sourceAlgebra: Algebra[A],
      targetAlgebra: Algebra[B],
      arrow: A > B
    ): Boolean =
      if (sourceAlgebra.carrier != arrow.source ||
          targetAlgebra.carrier != arrow.target)
        bail("Source/target of arrow do not match algebra carriers")
      else
        operators forall {
          case op: ScalarConstant =>
            true

          case op: PrincipalConstant =>
            (
              for (
                srcConstant <- sourceAlgebra.operatorAssignments.lookup(op);
                tgtConstant <- targetAlgebra.operatorAssignments.lookup(op)
              ) yield {
                (arrow o srcConstant) == tgtConstant
              }) getOrElse bail(
                "Not found in source algebra: " + op.name
              )

          case op: AbstractUnaryOp => (
            for (
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
              (arrow o srcOp) == (tgtOp o arrow)
            }) getOrElse bail(
              "Not found in source algebra: " + op.name
            )

          case op: AbstractBinaryOp =>
            val square = sourceAlgebra.carrier.squared
            (for (
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
                (arrow o srcOp.arrow) == tgtOp(arrow o square.π0, arrow o square.π1)
              }) getOrElse bail(
                "Not found in source algebra: " + op.name
              )

          case op: AbstractRightScalarBinaryOp =>
            val carrierScalars = sourceAlgebra.carrier x scalars
            (for (
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
              (arrow o srcOp.arrow) ==
                tgtOp(
                  arrow o carrierScalars.π0,
                  carrierScalars.π1
                )
            }) getOrElse bail(
                "Not found in source algebra: " + op.name
              )

          case op: AbstractScalarBinaryOp =>
            true // free pass, no need to verify these on carrier

          case op =>
            bail(
              s"Unknown type of operator, can't verify: ${op.name}"
            )
        }

    class Algebra[T <: ~](
      val carrier: DOT[T]
    )(
      assignments: OperatorAssignment[T, _ <: ~]*
    ) { algebra =>
      val operatorAssignments =
        OperatorAssignments(
          (preassignments ++ assignments) map {
            _.asInstanceOf[OperatorAssignment[T, S]]
          }
        )

      object EvaluationContext {
        def apply[T <: ~](
          variables: Seq[VariableTerm[_ <: AlgebraicSort]]
        ) =
          variables.foldRight(
            new SimpleEvaluationContext : EvaluationContext
          )(
            addVariableToContext
          )

        private def addVariableToContext(
          variable: VariableTerm[_ <: AlgebraicSort],
          context: EvaluationContext
        ) =
          new CompoundEvaluationContext(
            variable.symbol,
            carrierFor(variable),
            context
          )

        private def carrierFor(
          variable: VariableTerm[_ <: AlgebraicSort]
        ) =
          if (variable.isScalar)
            scalars
          else
            carrier
      }

      trait EvaluationContext {
        type ROOT <: ~
        def root: DOT[ROOT]
        def evaluate(
          term: Term[Principal]
        ): ROOT > T
        def evaluateScalar(
          term: Term[Scalar]
        ): ROOT > S
      }

      class SimpleEvaluationContext extends EvaluationContext {
        override type ROOT = UNIT
        override def root = I

        override def evaluate(
          term: Term[Principal]
        ): UNIT > T =
          term match {
            case term: PrincipalConstant =>
              operatorAssignments.lookup(term) map { constant =>
                constant o root.toI
              } getOrElse bail(
                "Unknown constant in expression: " + term.name
              )
            case _ =>
              bail(
                "No variables available for principal term: " + term
              )
          }

        override def evaluateScalar(
          term: Term[Scalar]
        ): UNIT > S =
          term match {
            case term: ScalarConstant =>
              operatorAssignments.lookup(term) map { constant =>
                constant o root.toI
              } getOrElse bail(
                  "Unknown constant in expression: " + term.name
                )

            case term @ BinaryScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op) map { op =>
                root(scalars) { r =>
                  op(
                    evaluateScalar(left)(r),
                    evaluateScalar(right)(r)
                  )
                }
              } getOrElse bail(
                  "Unknown operator in expression: " + op
                )

            case _ =>
              bail(
                "No variables available for scalar term: " + term
              )
          }
      }

      class CompoundEvaluationContext[HEAD <: ~](
        name: String,
        head: DOT[HEAD],
        val tail: EvaluationContext
      ) extends EvaluationContext {
        private type TAIL = tail.ROOT
        override type ROOT = HEAD x TAIL
        override def root : BIPRODUCT[HEAD, TAIL] = head x tail.root

        override def evaluate(term: Term[Principal]): HEAD x TAIL > T =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              root.π0.asInstanceOf[HEAD x TAIL > T]

            case term @ BinaryOpTerm(left, op, right) =>
              operatorAssignments.lookup(op) map { op =>
                root(carrier) { r =>
                  op(
                    evaluate(left)(r),
                    evaluate(right)(r)
                  )
                }
              } getOrElse bail(
                  "Unknown operator in expression: " + op
                )

            case term @ BinaryRightScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op) map { op =>
                root(carrier) { r =>
                  op(
                    evaluate(left)(r),
                    evaluateScalar(right)(r)
                  )
                }
              } getOrElse bail(
                  "Unknown operator in expression: " + op
                )

            case term @ UnaryOpTerm(op, innerTerm) =>
              operatorAssignments.lookup(op) map { op =>
                root(carrier) { r =>
                  op(evaluate(innerTerm)(r))
                }
              } getOrElse bail(
                "Unknown operator in expression: " + op
              )

            case _ =>
              tail.evaluate(term) o root.π1
          }

        override def evaluateScalar(term: Term[Scalar]): HEAD x TAIL > S =
          term match {
            case term: ScalarConstant =>
              operatorAssignments.lookup(term) map { constant =>
                constant o root.toI
              } getOrElse bail(
                "Unknown constant in expression: " + term.name
              )

            case VariableTerm(symbol, _) if symbol == name =>
              root.π0.asInstanceOf[HEAD x TAIL > S]

            case term @ BinaryScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op) map { op =>
                root(scalars) { r =>
                  op(
                    evaluateScalar(left)(r),
                    evaluateScalar(right)(r)
                  )
                }
              } getOrElse bail(
                "Unknown operator in expression: " + op
              )

            case _ =>
              tail.evaluateScalar(term) o root.π1
          }
      }

      def sanityTest() =
        if (!operatorAssignments.hasPrecisely(operators))
          bail("Assignments do not match signature of theory")
        else
          laws foreach { law =>
            if (!satisfies(law))
              law.fails
          }

      def satisfies(law: Law) =
        law isSatisfiedIn
          EvaluationContext(law.freeVariables)
    }
  }

  object AlgebraicTheory {
    def apply(operators: Operator*)(laws: Law*) =
      new AlgebraicTheory[UNIT](I)()(operators:_*)(laws:_*)
  }

  object AlgebraicTheoryWithScalars {
    def apply[S <: ~](
      scalars: DOT[S]
    )(
      preassignments: OperatorAssignment[_ <: ~, _ <: ~]*
    )(
      operators: Operator*
    )(
      laws: Law*
    ) =
      new AlgebraicTheory[S](scalars)(preassignments :_*)(operators :_*)(laws:_*)
  }

  type Algebra = AlgebraicTheory[_ <: ~]#Algebra[_ <: ~]
}



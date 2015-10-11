package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos.BaseTopos

import scala.language.{dynamics, implicitConversions, postfixOps}

sealed trait AlgebraicSort
class Principal extends AlgebraicSort
class Scalar extends AlgebraicSort

trait AlgebraicMachinery { topos: BaseTopos =>

  type NullaryOp[X <: ~] = ARROW[UNIT, X]
  type UnaryOp[X <: ~] = ARROW[X, X]
  type BinaryOp[X <: ~] = BiArrow[X, X, X]
  type RightScalarBinaryOp[X <: ~, S <: ~] = BiArrow[X, S, X]

  case class Law(left: Term[Principal], right: Term[Principal], name: Option[String] = None) {
    def isSatisfiedIn(context: Algebra#EvaluationContext) =
      context.evaluate(left) == context.evaluate(right)

    def fails =
      throw new IllegalArgumentException(name.getOrElse("Unnamed") + " law failed")

    val freeVariables = 
      (left.freeVariables ++ right.freeVariables) distinct

    def named(name: String) =
      Law(left, right, Some(name))
  }

  object NamedLaws {
    implicit class NamedLaw(name: String) {
      def law(unnamedLaw: Law) = unnamedLaw.named(name)
    }
  }

  sealed trait Term[X <: AlgebraicSort] extends Dynamic {
    def applyDynamic(name: String)(other: Term[X]) =
      BinaryOpTerm(this, StandardTermsAndOperators.binaryOpFrom(name), other)

    def +(other: Term[X]) =
      applyDynamic("+")(other)

    def *(other: Term[X]) =
      applyDynamic("*")(other)

    def **(other: Term[Scalar])(implicit eq: =:=[X, Principal]) =
      BinaryRightScalarOpTerm(
        this.asInstanceOf[Term[Principal]], // cast justified by =:=
        StandardTermsAndOperators.**,
        other
      )

    def ***(other: Term[Scalar])(implicit eq: =:=[X, Scalar]) =
      BinaryScalarOpTerm(
        this.asInstanceOf[Term[Scalar]], // cast justified by =:=
        StandardTermsAndOperators.***,
        other
      )

    def unary_- : Term[X] =
      UnaryOpTerm(StandardTermsAndOperators.-, this)
    val freeVariables : Seq[VariableTerm[_ <: AlgebraicSort]]
    def :=(that: Term[Principal])(implicit eq: =:=[X, Principal]) =
      Law(this.asInstanceOf[Term[Principal]], that) // cast justified by =:=
  }

  case class Operator(name: String, arity: Int)

  case class VariableTerm[S <: AlgebraicSort](symbol: String, isScalar : Boolean) extends Term[S] { term =>
    override val freeVariables = Seq(term)
  }

  case class BinaryOpTerm[S <: AlgebraicSort](
    left: Term[S],
    op: AbstractBinaryOp,
    right: Term[S]
  ) extends Term[S] {
    override val freeVariables = (left.freeVariables ++ right.freeVariables).distinct
  }

  case class BinaryRightScalarOpTerm(
    left: Term[Principal],
    op: AbstractRightScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Principal] {
    override val freeVariables = (left.freeVariables ++ right.freeVariables).distinct
  }

  case class BinaryScalarOpTerm(
    left: Term[Scalar],
    op: AbstractScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Scalar] {
    override val freeVariables = (left.freeVariables ++ right.freeVariables).distinct
  }

  case class UnaryOpTerm[S <: AlgebraicSort](
    op: AbstractUnaryOp,
    term: Term[S]
  ) extends Term[S] {
    override val freeVariables = term.freeVariables
  }

  class GeneralConstant[X <: AlgebraicSort](name: String) extends Operator(name, 0) with Term[X] {
    override val freeVariables = Nil
  }

  class PrincipalConstant(name: String) extends GeneralConstant[Principal](name) {
    def :=[T <: ~](nullaryOp: NullaryOp[T]) =
      new OperatorAssignment[T, ~](this) {
        override def lookupPrincipalConstant = Some(nullaryOp)
      }
  }

  class ScalarConstant(name: String) extends GeneralConstant[Scalar](name) {
    def :=[T <: ~](nullaryOp: NullaryOp[T]) =
      new OperatorAssignment[~, T](this) {
        override def lookupScalarConstant = Some(nullaryOp)
      }
  }

  case class OperatorAssignment[T <: ~, S <: ~](operator: Operator) {
    def lookupPrincipalConstant: Option[NullaryOp[T]] = None
    def lookupScalarConstant: Option[NullaryOp[S]] = None
    def lookupUnaryOp: Option[UnaryOp[T]] = None
    def lookupBinaryOp: Option[BinaryOp[T]] = None
    def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[T, S]] = None
    def lookupScalarBinaryOp: Option[BinaryOp[S]] = None
  }

  case class OperatorAssignments[T <: ~, S <: ~](assignments: Seq[OperatorAssignment[T, S]]) {
    def lookup(constant: PrincipalConstant): Option[NullaryOp[T]] = (
      for (assignment <- assignments if assignment.operator == constant)
        yield assignment.lookupPrincipalConstant
    ).headOption.flatten

    def lookup(constant: ScalarConstant): Option[NullaryOp[S]] = (
      for (assignment <- assignments if assignment.operator == constant)
        yield assignment.lookupScalarConstant
    ).headOption.flatten

    def lookup(op: AbstractUnaryOp): Option[UnaryOp[T]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupUnaryOp
    ).headOption.flatten

    def lookup(op: AbstractBinaryOp): Option[BinaryOp[T]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupBinaryOp
    ).headOption.flatten

    def lookup(op: AbstractRightScalarBinaryOp): Option[RightScalarBinaryOp[T, S]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupRightScalarBinaryOp
      ).headOption.flatten

    def lookup(op: AbstractScalarBinaryOp): Option[BinaryOp[S]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupScalarBinaryOp
      ).headOption.flatten

    def hasPrecisely(
      operators: Seq[Operator]
    ): Boolean =
        assignments.map { _.operator }.toSet ==
          operators.toSet
  }

  class AbstractBinaryOp(name: String) extends Operator(name, 2) {
    def :=[T <: ~](binaryOp: BinaryOp[T]) =
      new OperatorAssignment[T, ~](this){
        override def lookupBinaryOp = Some(binaryOp)
      }
  }

  class AbstractRightScalarBinaryOp(name: String) extends Operator(name, 2) {
    def :=[T <: ~, S <: ~](binaryOp: RightScalarBinaryOp[T, S]) =
      new OperatorAssignment[T, S](this){
        override def lookupRightScalarBinaryOp = Some(binaryOp)
      }
  }

  class AbstractScalarBinaryOp(name: String) extends Operator(name, 2) {
    def :=[S <: ~](binaryOp: BinaryOp[S]) =
      new OperatorAssignment[~, S](this){
        override def lookupScalarBinaryOp = Some(binaryOp)
      }
  }

  class AbstractUnaryOp(name: String) extends Operator(name, 1) {
    def :=[T <: ~](unaryOp: UnaryOp[T]) =
      new OperatorAssignment[T, ~](this){
        override def lookupUnaryOp = Some(unaryOp)
      }
  }

  object StandardTermsAndOperators {
    val o = new PrincipalConstant("o")
    val ι = new PrincipalConstant("ι")
    val II = new ScalarConstant("II")

    val α = VariableTerm[Principal]("α", false)
    val β = VariableTerm[Principal]("β", false)
    val γ = VariableTerm[Principal]("γ", false)
    val Φ = VariableTerm[Scalar]("Φ", true)
    val Ψ = VariableTerm[Scalar]("Ψ", true)

    val - = new AbstractUnaryOp("-")

    val * = new AbstractBinaryOp("*")
    val + = new AbstractBinaryOp("+")
    val ** = new AbstractRightScalarBinaryOp("**")
    val *** = new AbstractScalarBinaryOp("***")

    private val binaryOperators = Map[String, AbstractBinaryOp](
      "*" -> *,
      "+" -> $plus
    )
    def binaryOpFrom(name: String) =
      binaryOperators.getOrElse(name,
        throw new IllegalArgumentException("Unknown binary operator: " + name)
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
      arrow: ARROW[A, B]
    ): Boolean = {
      if (sourceAlgebra.carrier != arrow.source ||
          targetAlgebra.carrier != arrow.target)
        throw new IllegalArgumentException("Source/target of arrow do not match algebra carriers")
      else
        operators forall {
          case op: ScalarConstant => true

          case op: PrincipalConstant =>
            (
              for (
                srcConstant <- sourceAlgebra.operatorAssignments.lookup(op);
                tgtConstant <- targetAlgebra.operatorAssignments.lookup(op)
              ) yield {
                (arrow o srcConstant) == tgtConstant
              }).getOrElse {
                throw new IllegalArgumentException("Not found in source algebra: " + op.name)
              }

          case op: AbstractUnaryOp => (
            for (
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
              (arrow o srcOp) == (tgtOp o arrow)
            }).getOrElse {
              throw new IllegalArgumentException("Not found in source algebra: " + op.name)
            }

          case op: AbstractBinaryOp =>
            val square = sourceAlgebra.carrier.squared
            (for (
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
                (arrow o srcOp.arrow) == tgtOp(arrow o square.π0, arrow o square.π1)
              }).getOrElse {
              throw new IllegalArgumentException("Not found in source algebra: " + op.name)
            }

          case op: AbstractRightScalarBinaryOp =>
            val carrierScalars = sourceAlgebra.carrier x scalars
            (for (
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
              (arrow o srcOp.arrow) == tgtOp(arrow o carrierScalars.π0, carrierScalars.π1)
            }).getOrElse {
              throw new IllegalArgumentException("Not found in source algebra: " + op.name)
            }

          case op: AbstractScalarBinaryOp =>
            true // free pass, no need to verify these on carrier

          case op =>
            throw new IllegalArgumentException(s"Unknown type of operator, can't verify: ${op.name}")
        }
    }

    class Algebra[T <: ~](
      val carrier: DOT[T]
    )(assignments: OperatorAssignment[_ <: ~, _ <: ~]*) { algebra =>
      val operatorAssignments = OperatorAssignments(
        (preassignments ++ assignments) map {
        _.asInstanceOf[OperatorAssignment[T, S]]
      })

      object EvaluationContext {
        def apply[T <: ~](variables: Seq[VariableTerm[_ <: AlgebraicSort]]) =
          variables.foldRight(new SimpleEvaluationContext : EvaluationContext)(addVariableToContext)

        private def addVariableToContext(variable: VariableTerm[_ <: AlgebraicSort], context: EvaluationContext) =
            new CompoundEvaluationContext(variable.symbol, carrierFor(variable), context)

        private def carrierFor(variable: VariableTerm[_ <: AlgebraicSort]) =
          if (variable.isScalar)
            scalars
          else
            carrier
      }

      trait EvaluationContext {
        type ROOT <: ~
        def root: DOT[ROOT]
        def evaluate(term: Term[Principal]): ARROW[ROOT, T]
        def evaluateScalar(term: Term[Scalar]): ARROW[ROOT, S]
      }

      class SimpleEvaluationContext extends EvaluationContext {
        override type ROOT = UNIT
        override def root = I

        override def evaluate(term: Term[Principal]): ARROW[UNIT, T] =
          term match {
            case term: PrincipalConstant =>
              operatorAssignments.lookup(term).map { constant =>
                constant o root.toI
              }.getOrElse {
                throw new IllegalArgumentException("Unknown constant in expression: " + term.name)
              }
            case _ =>
              throw new IllegalArgumentException("No variables available for principal term: " + term)
          }

        override def evaluateScalar(term: Term[Scalar]): ARROW[UNIT, S] =
          term match {
            case term: ScalarConstant =>
              operatorAssignments.lookup(term).map { constant =>
                constant o root.toI
              }.getOrElse {
                throw new IllegalArgumentException("Unknown constant in expression: " + term.name)
              }

            case term @ BinaryScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op).map { op =>
                root(scalars) { r =>
                  op(
                    evaluateScalar(left)(r),
                    evaluateScalar(right)(r)
                  )
                }
              }.getOrElse {
                throw new IllegalArgumentException("Unknown operator in expression: " + op)
              }

            case _ =>
              throw new IllegalArgumentException("No variables available for scalar term: " + term)
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

        override def evaluate(term: Term[Principal]): ARROW[HEAD x TAIL, T] =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              root.π0.asInstanceOf[ARROW[HEAD x TAIL, T]]

            case term @ BinaryOpTerm(left, op, right) =>
              operatorAssignments.lookup(op).map { op =>
                root(carrier) { r =>
                  op(
                    evaluate(left)(r),
                    evaluate(right)(r)
                  )
                }
              }.getOrElse {
                throw new IllegalArgumentException("Unknown operator in expression: " + op)
              }

            case term @ BinaryRightScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op).map { op =>
                root(carrier) { r =>
                  op(
                    evaluate(left)(r),
                    evaluateScalar(right)(r)
                  )
                }
              }.getOrElse {
                throw new IllegalArgumentException("Unknown operator in expression: " + op)
              }

            case term @ UnaryOpTerm(op, innerTerm) =>
              operatorAssignments.lookup(op).map { op =>
                root(carrier) { r =>
                  op(evaluate(innerTerm)(r))
                }
              }.getOrElse {
                throw new IllegalArgumentException("Unknown operator in expression: " + op)
              }
            case _ =>
              tail.evaluate(term) o root.π1
          }

        override def evaluateScalar(term: Term[Scalar]): ARROW[HEAD x TAIL, S] =
          term match {
            case term: ScalarConstant =>
              operatorAssignments.lookup(term).map { constant =>
                constant o root.toI
              }.getOrElse {
                throw new IllegalArgumentException("Unknown constant in expression: " + term.name)
              }

            case VariableTerm(symbol, _) if symbol == name =>
              root.π0.asInstanceOf[ARROW[HEAD x TAIL, S]]

            case term @ BinaryScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op).map { op =>
                root(scalars) { r =>
                  op(
                    evaluateScalar(left)(r),
                    evaluateScalar(right)(r)
                  )
                }
              }.getOrElse {
                throw new IllegalArgumentException("Unknown operator in expression: " + op)
              }

            case _ =>
              tail.evaluateScalar(term) o root.π1
          }
      }

      def sanityTest =
        if (!operatorAssignments.hasPrecisely(operators))
          throw new IllegalArgumentException("Assignments do not match signature of theory")
        else
          laws foreach { law =>
            if (!satisfies(law))
              law.fails
          }

      def satisfies(law: Law) =
        law.isSatisfiedIn(EvaluationContext(law.freeVariables))
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



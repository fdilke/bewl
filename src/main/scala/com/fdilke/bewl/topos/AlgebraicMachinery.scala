package com.fdilke.bewl.topos

import com.fdilke.bewl.fsets.FiniteSets.FiniteSetsArrow

import scala.language.implicitConversions
import scala.language.dynamics
import scala.language.postfixOps
import scala.reflect._

sealed trait AlgebraicSort
class Principal extends AlgebraicSort
class Scalar extends AlgebraicSort

trait AlgebraicMachinery { topos: BaseTopos =>

  type NullaryOp[X <: ~] = ARROW[UNIT, X]
  type UnaryOp[X <: ~] = ARROW[X, X]
  type BinaryOp[X <: ~] = BiArrow[X, X, X]
  type RightScalarBinaryOp[X <: ~, S <: ~] = BiArrow[X, S, X]

  case class Law(left: Term[Principal], right: Term[Principal]) {
    val freeVariables = 
      (left.freeVariables ++ right.freeVariables) distinct
  }

  sealed trait Term[X <: AlgebraicSort] extends Dynamic {
    def applyDynamic(name: String)(other: Term[X]) =
      BinaryOpTerm(this, StandardTermsAndOperators.binaryOpFrom(name), other)

    def +(other: Term[X]) =
      applyDynamic("+")(other)

    def **(other: Term[Scalar])(implicit eq: =:=[X, Principal]) =
      BinaryScalarOpTerm(
        this.asInstanceOf[Term[Principal]],
        StandardTermsAndOperators.**,
        other
      )

    def unary_- : Term[X] =
      UnaryOpTerm(StandardTermsAndOperators.-, this)
    val freeVariables : Seq[VariableTerm[_ <: AlgebraicSort]]
    def :=(that: Term[Principal])(implicit eq: =:=[X, Principal]) =
      Law(this.asInstanceOf[Term[Principal]], that) // cast justified by =:=
  }

  case class Operator(name: String, arity: Int)

  case class VariableTerm[S <: AlgebraicSort : ClassTag](symbol: String) extends Term[S] { term =>
    override val freeVariables = Seq(term)
    val tag = classTag[S]
  }

  case class BinaryOpTerm[S <: AlgebraicSort](
    left: Term[S],
    op: AbstractBinaryOp,
    right: Term[S]
  ) extends Term[S] {
    override val freeVariables = (left.freeVariables ++ right.freeVariables).distinct
  }

  case class BinaryScalarOpTerm(
    left: Term[Principal],
    op: AbstractRightScalarBinaryOp,
    right: Term[Scalar]
  ) extends Term[Principal] {
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
    def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[T, _ <: ~]] = None
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

    def lookup(op: AbstractRightScalarBinaryOp): Option[RightScalarBinaryOp[T, _ <: ~]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupRightScalarBinaryOp
      ).headOption.flatten

    def hasPrecisely(
      constants: Seq[GeneralConstant[_ <: AlgebraicSort]],
      operators: Seq[Operator]
    ): Boolean =
        assignments.map { _.operator }.toSet ==
          (operators ++ constants).toSet
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

  class AbstractUnaryOp(name: String) extends Operator(name, 1) {
    def :=[T <: ~](unaryOp: UnaryOp[T]) =
      new OperatorAssignment[T, ~](this){
        override def lookupUnaryOp = Some(unaryOp)
      }
  }

  object StandardTermsAndOperators {
    val O = new PrincipalConstant("O")
    val II = new ScalarConstant("II")

    val α = VariableTerm[Principal]("α")
    val β = VariableTerm[Principal]("β")

    val - = new AbstractUnaryOp("-")

    val * = new AbstractBinaryOp("*")
    val + = new AbstractBinaryOp("+")
    val ** = new AbstractRightScalarBinaryOp("**")

    private val binaryOperators = Map[String, AbstractBinaryOp](
      "*" -> *,
      "+" -> $plus
    )
    def binaryOpFrom(name: String) =
      binaryOperators.getOrElse(name,
        throw new IllegalArgumentException("Unknown binary operator: ")
      )
  }

  // TODO: try a curried constructor here, with the scalars at the beginning
  class AlgebraicTheory[S <: ~](
      scalars: DOT[S]
   )(
      constants: GeneralConstant[_ <: AlgebraicSort]*
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
    ) = {
      if (sourceAlgebra.carrierFor(classTag[Principal]) != arrow.source ||
          targetAlgebra.carrierFor(classTag[Principal]) != arrow.target) {
        throw new IllegalArgumentException("Source/target of arrow do not match algebra carriers")
      }
      operators forall {
        case op: AbstractUnaryOp => (
          for (srcOp <- sourceAlgebra.operatorAssignments.lookup(op);
               tgtOp <- targetAlgebra.operatorAssignments.lookup(op))
            yield (tgtOp o arrow) == (arrow o srcOp)
          ).getOrElse {
          throw new IllegalArgumentException("Not found in source algebra: " + op.name)
        }
      }
    }

    class Algebra[T <: ~](
      carrier: DOT[T]
    )(assignments: OperatorAssignment[_ <: ~, _ <: ~]*) { algebra =>
      val operatorAssignments = OperatorAssignments(
        (preassignments ++ assignments) map {
        _.asInstanceOf[OperatorAssignment[T, S]]
      })

      object EvaluationContext {
        def apply[T <: ~](variables: Seq[VariableTerm[_ <: AlgebraicSort]]): EvaluationContext[_ <: ~] =
          variables match {
            case Nil => new SimpleEvaluationContext
            case head :: tail =>
              new CompoundEvaluationContext(head.symbol, carrierFor(head.tag), EvaluationContext(tail))
              // TODO: store the variables in context
              // TODO: refactor as fold
          }
      }

      sealed trait EvaluationContext[R <: ~] {
        def root: DOT[R]
        def evaluate(term: Term[Principal]): ARROW[R, T]
        def evaluateScalar(term: Term[Scalar]): ARROW[R, S]
      }

      class SimpleEvaluationContext extends EvaluationContext[UNIT] {
        override def root: DOT[UNIT] = I

        override def evaluate(term: Term[Principal]): ARROW[UNIT, T] =
          term match {
            case term: PrincipalConstant =>
              operatorAssignments.lookup(term).map { constant =>
                constant o root.toI
              }.getOrElse {
                throw new IllegalArgumentException("Unknown constant in expression: " + term.name)
              }
            case _ =>
              throw new IllegalArgumentException("No variables available")
          }

        override def evaluateScalar(term: Term[Scalar]): ARROW[UNIT, S] =
          throw new IllegalArgumentException("No variables available")
      }

      class CompoundEvaluationContext[HEAD <: ~, TAIL <: ~](
        name: String,
        head: DOT[HEAD],
        tail: EvaluationContext[TAIL]
      ) extends EvaluationContext[HEAD x TAIL] {
        override def root : BIPRODUCT[HEAD, TAIL] = head x tail.root

        override def evaluate(term: Term[Principal]): ARROW[HEAD x TAIL, T] =
          term match {
            case VariableTerm(symbol) if symbol == name =>
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
            case term @ BinaryScalarOpTerm(left, op, right) =>
              operatorAssignments.lookup(op).map { op =>
                root(carrier) { r =>
                  op.asInstanceOf[RightScalarBinaryOp[T, S]](
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

            case _ => ???
          }
      }

      def carrierFor[S <: AlgebraicSort](tag: ClassTag[S]): DOT[T] =
        carrier // TODO: support other type lookups later

      def sanityTest =
        if (!operatorAssignments.hasPrecisely(constants, operators))
          throw new IllegalArgumentException("Assignments do not match signature of theory")
        else
          laws foreach { law =>
            val context = EvaluationContext(law.freeVariables)
            if (context.evaluate(law.left) != context.evaluate(law.right)) {
              throw new IllegalArgumentException("Unnamed law failed")
            }
        }
    }
  }

  object AlgebraicTheory {
    def apply(constants: GeneralConstant[_ <: AlgebraicSort]*)(operators: Operator*)(laws: Law*) =
      new AlgebraicTheory[UNIT](I)(constants :_*)()(operators :_*)(laws:_*)
  }

  object AlgebraicTheoryWithScalars {
    def apply[S <: ~](
      scalars: DOT[S]
    )(
      constants: GeneralConstant[_ <: AlgebraicSort]*
    )(
      preassignments: OperatorAssignment[_ <: ~, _ <: ~]*
    )(
      operators: Operator*
    )(
      laws: Law*
    ) =
      new AlgebraicTheory[S](scalars)(constants :_*)(preassignments :_*)(operators :_*)(laws:_*)
  }
}



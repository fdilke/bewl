package com.fdilke.bewl.topos

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

  sealed trait Term[S <: AlgebraicSort] extends Dynamic {
    def applyDynamic(name: String)(that: Term[S]) =
      BinaryOpTerm(this, StandardTermsAndOperators.binaryOpFrom(name), that)

    def +(other: Term[S]) =
      applyDynamic("+")(other)

    def unary_- : Term[S] =
      UnaryOpTerm(StandardTermsAndOperators.-, this)
    val freeVariables : Seq[VariableTerm[_ <: AlgebraicSort]]
    def :=(that: Term[Principal])(implicit eq: =:=[S, Principal]) =
      Law(this.asInstanceOf[Term[Principal]], that)
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

  case class UnaryOpTerm[S <: AlgebraicSort](
    op: AbstractUnaryOp,
    term: Term[S]
  ) extends Term[S] {
    override val freeVariables = term.freeVariables
  }

  class Constant(name: String) extends Operator(name, 0) with Term[Principal] {
    override val freeVariables = Nil
    def :=[T <: ~](nullaryOp: NullaryOp[T]) =
      new OperatorAssignment[T](this) {
        override def lookupConstant = Some(nullaryOp)
      }
  }

  case class OperatorAssignment[T <: ~](operator: Operator) {
    def lookupConstant: Option[NullaryOp[T]] = None
    def lookupUnaryOp: Option[UnaryOp[T]] = None
    def lookupBinaryOp: Option[BinaryOp[T]] = None
  }

  case class OperatorAssignments[T <: ~](assignments: Seq[OperatorAssignment[T]]) {
    def lookup(constant: Constant): Option[NullaryOp[T]] = (
      for (assignment <- assignments if assignment.operator == constant)
        yield assignment.lookupConstant
    ).headOption.flatten

    def lookup(op: AbstractUnaryOp): Option[UnaryOp[T]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupUnaryOp
    ).headOption.flatten

    def lookup(op: AbstractBinaryOp): Option[BinaryOp[T]] = (
      for (assignment <- assignments if assignment.operator == op)
        yield assignment.lookupBinaryOp
    ).headOption.flatten

    def hasPrecisely(constants: Seq[Constant], operators: Seq[Operator]): Boolean =
        assignments.map { _.operator }.toSet ==
          (operators ++ constants).toSet
  }

  class AbstractBinaryOp(name: String) extends Operator(name, 2) {
    def :=[T <: ~](binaryOp: BinaryOp[T]) =
      new OperatorAssignment[T](this){
        override def lookupBinaryOp = Some(binaryOp)
      }
  }

  class AbstractUnaryOp(name: String) extends Operator(name, 1) {
    def :=[T <: ~](unaryOp: UnaryOp[T]) =
      new OperatorAssignment[T](this){
        override def lookupUnaryOp = Some(unaryOp)
      }
  }

  object StandardTermsAndOperators {
    val O = new Constant("O")

    val α = VariableTerm[Principal]("α")
    val β = VariableTerm[Principal]("β")

    val - = new AbstractUnaryOp("-")

    val * = new AbstractBinaryOp("*")
    val + = new AbstractBinaryOp("+")

    private val binaryOperators = Map[String, AbstractBinaryOp](
      "*" -> *,
      "+" -> $plus
    )
    def binaryOpFrom(name: String) =
      binaryOperators.getOrElse(name,
        throw new IllegalArgumentException("Unknown binary operator: ")
      )
  }

  class AlgebraicTheory(constants: Seq[Constant], operators: Seq[Operator], laws: Seq[Law]) {
    class Algebra[T <: ~](carrier: DOT[T])(assignments: OperatorAssignment[T]*) { algebra =>
      val operatorAssignments = OperatorAssignments(assignments)

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
        val root: DOT[R]
        def evaluate(term: Term[Principal]): ARROW[R, T]
      }

      class SimpleEvaluationContext extends EvaluationContext[UNIT] {
        override val root: DOT[UNIT] = I

        override def evaluate(term: Term[Principal]): ARROW[UNIT, T] =
          ??? // carrier.fromO
      }

      class CompoundEvaluationContext[HEAD <: ~, TAIL <: ~](
        name: String,
        head: DOT[HEAD],
        tail: EvaluationContext[TAIL]
      ) extends EvaluationContext[HEAD x TAIL] {
        val realRoot : BIPRODUCT[HEAD, TAIL] = head x tail.root
        override val root : DOT[HEAD x TAIL] = realRoot

        override def evaluate(term: Term[Principal]): ARROW[HEAD x TAIL, T] =
          term match {
            case VariableTerm(symbol) =>
              if (symbol == name)
                realRoot.π0.asInstanceOf[ARROW[HEAD x TAIL, T]] // TODO: use =:= here to avoid cast?
              else
                tail.evaluate(term) o realRoot.π1
            case term: Constant =>
              operatorAssignments.lookup(term).map { constant =>
                constant o root.toI
              }.getOrElse {
                throw new IllegalArgumentException("Unknown constant in expression: " + term.name)
              }
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
            case term @ UnaryOpTerm(op, innerTerm) =>
              operatorAssignments.lookup(op).map { op =>
                root(carrier) { r =>
                  op(evaluate(innerTerm)(r))
                }
              }.getOrElse {
                throw new IllegalArgumentException("Unknown operator in expression: " + op)
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
    def apply(constants: Constant*)(operators: Operator*)(laws: Law*) =
      new AlgebraicTheory(constants, operators, laws)
  }
}



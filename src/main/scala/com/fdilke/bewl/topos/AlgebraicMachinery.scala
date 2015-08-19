package com.fdilke.bewl.topos

import scala.language.{dynamics, implicitConversions, postfixOps}
import scala.reflect._

sealed trait AlgebraicSort
class Principal extends AlgebraicSort
class Scalar extends AlgebraicSort

trait AlgebraicMachinery { topos: BaseTopos =>

  type NullaryOp[X <: ~] = ARROW[UNIT, X]
  type UnaryOp[X <: ~] = ARROW[X, X]
  type BinaryOp[X <: ~] = BiArrow[X, X, X]
  type RightScalarBinaryOp[X <: ~, S <: ~] = BiArrow[X, S, X]

  case class Law(left: Term[Principal], right: Term[Principal], name:Option[String] = None) {
    def isSatisfiedIn[S <: ~, T <: ~](context: AlgebraicTheory[S]#Algebra[T]#EvaluationContext) =
      context.evaluate(left) == context.evaluate(right)

    def fails =
      throw new IllegalArgumentException(name.getOrElse("Unnamed") + " law failed")

    val freeVariables = 
      (left.freeVariables ++ right.freeVariables) distinct

    def named(name: String) =
      Law(left, right, Some(name))
  }

  object NamedLaws {
    case class Widget(n: Int)

    implicit class NamedLaw(name: String) {
      def law(unnamedLaw: Law) = unnamedLaw.named(name)
    }
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
    ): Boolean = {
      if (sourceAlgebra.carrier != arrow.source ||
          targetAlgebra.carrier != arrow.target)
        throw new IllegalArgumentException("Source/target of arrow do not match algebra carriers")
      else
        (constants ++ operators) forall {
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
              srcOp0 <- sourceAlgebra.operatorAssignments.lookup(op);
              tgtOp0 <- targetAlgebra.operatorAssignments.lookup(op)
            ) yield {
              val srcOp = srcOp0.asInstanceOf[RightScalarBinaryOp[A, S]]
              val tgtOp = tgtOp0.asInstanceOf[RightScalarBinaryOp[B, S]]
              (arrow o srcOp.arrow) == tgtOp(arrow o carrierScalars.π0, carrierScalars.π1)
            }).getOrElse {
              throw new IllegalArgumentException("Not found in source algebra: " + op.name)
            }

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
        def apply[T <: ~](variables: Seq[VariableTerm[_ <: AlgebraicSort]]): EvaluationContext =
          variables.foldRight(new SimpleEvaluationContext : EvaluationContext) {
            (variable, context) => new CompoundEvaluationContext(variable.symbol, carrier, context)
          }
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
              throw new IllegalArgumentException("No variables available")
          }

        override def evaluateScalar(term: Term[Scalar]): ARROW[UNIT, S] =
          throw new IllegalArgumentException("No variables available")
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

      def sanityTest =
        if (!operatorAssignments.hasPrecisely(constants, operators))
          throw new IllegalArgumentException("Assignments do not match signature of theory")
        else
          laws foreach { law =>
            val context = EvaluationContext(law.freeVariables)
            if (!law.isSatisfiedIn(context))
              law.fails
        }

      def satisfies(law: Law) = {
        val context = EvaluationContext(law.freeVariables)
        law.isSatisfiedIn(context)
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



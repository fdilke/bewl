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

  sealed trait Term[
      X <: AlgebraicSort
  ] extends Dynamic {
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

    val freeVariables: Seq[VariableTerm[_ <: AlgebraicSort]]

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
      innerTerm: Term[S]
  ) extends Term[S] {
    override val freeVariables =
      innerTerm.freeVariables
  }

  class ConstantOperator[
      X <: AlgebraicSort
  ](
      name: String
  ) extends Operator(name, 0)
      with Term[X] {
    override val freeVariables =
      Nil
  }

  class PrincipalConstant(
      name: String
  ) extends ConstantOperator[Principal](
        name
      ) {
    def :=[
        S <: ~,
        T <: ~
    ](
        nullaryOp: NullaryOp[T]
    ) =
      new OperatorAssignment[T, S](this) {
        override def lookupPrincipalConstant =
          Some(nullaryOp)
      }
  }

  class ScalarConstant(
      name: String
  ) extends ConstantOperator[Scalar](
        name
      ) {
    def :=[
        S <: ~,
        T <: ~
    ](
        nullaryOp: NullaryOp[T]
    ) =
      new OperatorAssignment[S, T](this) {
        override def lookupScalarConstant =
          Some(nullaryOp)
      }
  }

  abstract case class OperatorAssignment[
      T <: ~,
      S <: ~
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
      lookupPrincipalConstant foreach { _.sanityTest }
      lookupUnaryOp foreach { _.sanityTest }
      lookupBinaryOp foreach { _.sanityTest }
      lookupScalarConstant foreach { _.sanityTest }
      lookupRightScalarBinaryOp foreach { _.sanityTest }
      lookupScalarBinaryOp foreach { _.sanityTest }
    }
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

    def crossedWith[U <: ~](
        that: OperatorAssignments[U, S],
        productCarrier: BIPRODUCT[T, U],
        scalars: DOT[S]
    ): Seq[OperatorAssignment[T x U, S]] =
      assignments map { assignment =>
        import assignment.operator
        that.doLookup(operator) { thatAssignment =>
          Some(
            new OperatorAssignment[T x U, S](operator) {
              override def lookupUnaryOp: Option[UnaryOp[T x U]] =
                for {
                  op <- assignment.lookupUnaryOp
                  thatOp <- thatAssignment.lookupUnaryOp
                } yield productCarrier(productCarrier) { tu =>
                  val t = productCarrier.π0(tu)
                  val u = productCarrier.π1(tu)
                  productCarrier.pair(
                    op(t),
                    thatOp(u)
                  )
                }

              override def lookupPrincipalConstant: Option[NullaryOp[T x U]] =
                for {
                  op <- assignment.lookupPrincipalConstant
                  thatOp <- thatAssignment.lookupPrincipalConstant
                } yield I(productCarrier) { __ =>
                  productCarrier.pair(
                    op(__),
                    thatOp(__)
                  )
                }

              override def lookupBinaryOp: Option[BinaryOp[T x U]] =
                for {
                  op <- assignment.lookupBinaryOp
                  thatOp <- thatAssignment.lookupBinaryOp
                } yield productCarrier.squared.biArrow(productCarrier) { (tu, vw) =>
                  val t = productCarrier.π0(tu)
                  val u = productCarrier.π1(tu)
                  val v = productCarrier.π0(vw)
                  val w = productCarrier.π1(vw)
                  productCarrier.pair(
                    op(t, v),
                    thatOp(u, w)
                  )
                }

              override def lookupRightScalarBinaryOp: Option[RightScalarBinaryOp[T x U, S]] =
                for {
                  op <- assignment.lookupRightScalarBinaryOp
                  thatOp <- thatAssignment.lookupRightScalarBinaryOp
                } yield (productCarrier x scalars).biArrow(productCarrier) { (tu, s) =>
                  val t = productCarrier.π0(tu)
                  val u = productCarrier.π1(tu)
                  productCarrier.pair(
                    op(t, s),
                    thatOp(u, s)
                  )
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
        } getOrElse {
          throw new IllegalArgumentException(
            "algebra multiplication failed: can't match operator " + operator
          )
        }
      }
  }

  class AbstractBinaryOp(
      name: String
  ) extends Operator(name, 2) {
    def :=[S <: ~, T <: ~](
        binaryOp: BinaryOp[T]
    ) =
      new OperatorAssignment[T, S](this) {
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
      new OperatorAssignment[T, S](this) {
        override def lookupRightScalarBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractScalarBinaryOp(
      name: String
  ) extends Operator(name, 2) {
    def :=[S <: ~, T <: ~](binaryOp: BinaryOp[S]) =
      new OperatorAssignment[T, S](this) {
        override def lookupScalarBinaryOp =
          Some(binaryOp)
      }
  }

  class AbstractUnaryOp(
      name: String
  ) extends Operator(name, 1) {
    def :=[S <: ~, T <: ~](unaryOp: UnaryOp[T]) =
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
        "+" -> $plus,
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
      S <: ~
  ](
      scalars: DOT[S]
  )(
      preassignments: OperatorAssignment[_ <: ~, S]*
  )(
      operators: Operator*
  )(
      laws: Law*
  ) {
    def extend(moreOperators: Operator*)(moreLaws: Law*) =
      new AlgebraicTheory[S](
        scalars
      )(
        preassignments: _*
      )(
        operators ++ moreOperators: _*
      )(
        laws ++ moreLaws: _*
      )

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
              for {
                srcConstant <- sourceAlgebra.operatorAssignments.lookup(op)
                tgtConstant <- targetAlgebra.operatorAssignments.lookup(op)
              } yield (arrow o srcConstant) == tgtConstant
            ) getOrElse bail(
              "Not found in source algebra: " + op.name
            )

          case op: AbstractUnaryOp =>
            (
              for {
                srcOp <- sourceAlgebra.operatorAssignments.lookup(op)
                tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
              } yield (arrow o srcOp) == (tgtOp o arrow)
            ) getOrElse bail(
              "Not found in source algebra: " + op.name
            )

          case op: AbstractBinaryOp =>
            val square = sourceAlgebra.carrier.squared
            (
              for {
                srcOp <- sourceAlgebra.operatorAssignments.lookup(op)
                tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
              } yield (arrow o srcOp.arrow) ==
                tgtOp(
                  arrow o square.π0,
                  arrow o square.π1
                )
            ) getOrElse bail(
              "Not found in source algebra: " + op.name
            )

          case op: AbstractRightScalarBinaryOp =>
            val carrierScalars = sourceAlgebra.carrier x scalars
            (for {
              srcOp <- sourceAlgebra.operatorAssignments.lookup(op)
              tgtOp <- targetAlgebra.operatorAssignments.lookup(op)
            } yield (arrow o srcOp.arrow) ==
              tgtOp(
                arrow o carrierScalars.π0,
                carrierScalars.π1
              )) getOrElse bail(
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
        private val assignments: OperatorAssignment[T, S]*
    ) { algebra =>
      val operatorAssignments =
        OperatorAssignments(
          (preassignments map {
            _.asInstanceOf[OperatorAssignment[T, S]]
          }) ++ assignments
        )

      def x[U <: ~](
          that: Algebra[U]
      ): Algebra[T x U] = {
        val productCarrier = carrier x that.carrier
        new Algebra(
          productCarrier
        )(
          OperatorAssignments(
            assignments
          ).crossedWith(
            OperatorAssignments(that.assignments),
            productCarrier,
            scalars
          ): _*
        )
      }

      object EvaluationContext {
        def apply[T <: ~](
            variables: Seq[VariableTerm[_ <: AlgebraicSort]]
        ) =
          variables.foldRight(
            new SimpleEvaluationContext: EvaluationContext
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

        protected def evaluateScalarConstant(
            term: ScalarConstant
        ): ROOT > S =
          operatorAssignments.lookup(term) map { constant =>
            constant o root.toI
          } getOrElse bail(
            "Unknown constant in expression: " + term.name
          )

        protected def evaluatePrincipalConstant(
            term: PrincipalConstant
        ): ROOT > T =
          operatorAssignments.lookup(term) map { constant =>
            constant o root.toI
          } getOrElse bail(
            "Unknown constant in expression: " + term.name
          )

        protected def evaluateBinaryScalarOpTerm(
            term: BinaryScalarOpTerm
        ): ROOT > S =
          operatorAssignments.lookup(term.op) map { op =>
            root(scalars) { r =>
              op(
                evaluateScalar(term.left)(r),
                evaluateScalar(term.right)(r)
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateBinaryOpTerm(
            term: BinaryOpTerm[Principal]
        ): ROOT > T =
          operatorAssignments.lookup(term.op) map { op =>
            root(carrier) { r =>
              op(
                evaluate(term.left)(r),
                evaluate(term.right)(r)
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateBinaryRightScalarOpTerm(
            term: BinaryRightScalarOpTerm
        ): ROOT > T =
          operatorAssignments.lookup(term.op) map { op =>
            root(carrier) { r =>
              op(
                evaluate(term.left)(r),
                evaluateScalar(term.right)(r)
              )
            }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )

        protected def evaluateUnaryOpTerm(
            term: UnaryOpTerm[Principal]
        ): ROOT > T =
          operatorAssignments.lookup(term.op) map { op =>
            root(carrier) { r => op(evaluate(term.innerTerm)(r)) }
          } getOrElse bail(
            "Unknown operator in expression: " + term.op
          )
      }

      class SimpleEvaluationContext extends EvaluationContext {
        override type ROOT = UNIT
        override def root = I

        override def evaluate(
            term: Term[Principal]
        ): UNIT > T =
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
        ): UNIT > S =
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

      class CompoundEvaluationContext[HEAD <: ~](
          name: String,
          head: DOT[HEAD],
          val tail: EvaluationContext
      ) extends EvaluationContext {
        private type TAIL = tail.ROOT
        override type ROOT = HEAD x TAIL
        override def root: BIPRODUCT[HEAD, TAIL] =
          head x tail.root

        override def evaluate(
            term: Term[Principal]
        ): HEAD x TAIL > T =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              root.π0.asInstanceOf[HEAD x TAIL > T]

            case term: BinaryOpTerm[Principal] =>
              evaluateBinaryOpTerm(term)

            case term: BinaryRightScalarOpTerm =>
              evaluateBinaryRightScalarOpTerm(term)

            case term: UnaryOpTerm[Principal] =>
              evaluateUnaryOpTerm(term)

            case _ =>
              tail.evaluate(term) o root.π1
          }

        override def evaluateScalar(
            term: Term[Scalar]
        ): HEAD x TAIL > S =
          term match {
            case VariableTerm(symbol, _) if symbol == name =>
              root.π0.asInstanceOf[HEAD x TAIL > S]

            case term: BinaryScalarOpTerm =>
              evaluateBinaryScalarOpTerm(term)

            case _ =>
              tail.evaluateScalar(term) o root.π1
          }
      }

      def satisfies(law: Law) =
        law isSatisfiedIn
          EvaluationContext(
            law.freeVariables
          )

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

  object AlgebraicTheory {
    def apply(operators: Operator*)(laws: Law*) =
      new AlgebraicTheory[UNIT](I)()(operators: _*)(laws: _*)
  }

  object AlgebraicTheoryWithScalars {
    def apply[
        S <: ~
    ](
        scalars: DOT[S]
    )(
        preassignments: OperatorAssignment[_ <: ~, S]*
    )(
        operators: Operator*
    )(
        laws: Law*
    ) =
      new AlgebraicTheory[S](scalars)(preassignments: _*)(operators: _*)(laws: _*)
  }

  type Algebra = AlgebraicTheory[_ <: ~]#Algebra[_ <: ~]
}

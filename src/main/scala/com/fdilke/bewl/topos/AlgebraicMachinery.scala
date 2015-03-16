package com.fdilke.bewl.topos

import com.fdilke.bewl.topos.StarTag._
import scala.reflect.runtime.universe._
import scala.language.implicitConversions

trait AlgebraicMachinery { topos: BaseTopos =>

  // Multiproducts. TODO: split out as separate trait?

  case class TypedStar[X <: ~](star: DOT[X]) {
    type TYPE = X
  }

  abstract class OtherRichStar[X <: ~] {
    def typed: TypedStar[X]
  }

  implicit def enrichOther[X <: ~](star: DOT[X]) = new OtherRichStar[X] {
    override def typed = new TypedStar[X](star)
  }

  object MultiProduct {

    def apply(components: DOT[_ <: ~]*): MultiProduct[_] = components map { _.typed } match {
      case Seq() =>
        new MultiProduct[UNIT] {
          val root = I
          val projections = Seq.empty
        }

      case Seq(star: TypedStar[_]) =>
        new MultiProduct[star.TYPE]  {
          val root = star.star
          val projections = Seq(root.identity)
        }

//      case prevStars :+ star =>
//        val prevProduct = MultiProduct(prevStars map { _.star } :_*)
//        new MultiProduct[prevProduct.TYPE x star.TYPE]  {
//          val root = prevProduct.root x star.star
//          val projections = prevProduct.projections map { (p: ARROW[prevProduct.TYPE, _]) =>
//            p o root.π0
//          } :+  root.π1
//        }
    }
  }

  abstract class MultiProduct[A <: ~] {
    type TYPE = A
    val root: DOT[A]
    val projections: Seq[ARROW[A, _]]
  }

  // Multiproducts. end

  type NullaryOp[X <: ~] = ARROW[UNIT, X]
  type UnaryOp[X <: ~] = ARROW[X, X]
  type BinaryOp[X <: ~] = BiQuiver[X, X, X]
  type RightScalarBinaryOp[X <: ~, S <: ~] = BiQuiver[X, S, X]

  object AbstractOp {
    def unit: AbstractNullaryOp[Principal] = abstractNullaryOp("unit")
    def unitRightScalar = abstractNullaryOp("right scalar unit", rightScalar)
    def multiply = abstractBinaryOp("multiply")
    def unaryOperator = abstractUnaryOp("unaryOperator")
    def rightScalarMultiply = new AbstractRightScalarBinaryOp("right scalar multiply", principal, rightScalar)

    def abstractNullaryOp[A: TypeTag](name: String, starTag: StarTag[A] = principal) =
      new AbstractNullaryOp[A](name, starTag)

    def abstractUnaryOp[A: TypeTag](name: String, starTag: StarTag[A] = principal) =
      new AbstractUnaryOp[A](name, starTag)

    def abstractBinaryOp[A: TypeTag](name: String, starTag: StarTag[A] = principal) =
      new AbstractBinaryOp[A](name, starTag)
  }

  class Arity[A](tags: StarTag[_]*)
  object Arity {
    def apply() = new Arity[Unit]()
    def apply[A](tag: StarTag[A]) = new Arity[A](tag)
    def apply[A, B](tagA: StarTag[A], tagB: StarTag[B]) = new Arity[(A, B)](tagA, tagB)
  }

//  trait Signature[RETURN_TYPE]
  class AbstractOp[A, R](name: String, arity: Arity[A], returnTag: StarTag[R])

  class AbstractNullaryOp[A: TypeTag](name: String, starTag: StarTag[A]) extends
    AbstractOp[Unit, A](name, Arity(), starTag)
    with Variable[A] {
    def :=[X <: ~](op: NullaryOp[X]): OpAssignment[X] =
      NullaryOpAssignment(this, op)
    def evaluate[X <: ~](context: EvaluationContext[X]): ARROW[context.ROOT, X] =
      context.assignments.lookup(this) o context.root.toI
  }

  class AbstractUnaryOp[A: TypeTag](name: String, starTag: StarTag[A])
    extends AbstractOp[A, A](
      name,
      Arity(starTag), starTag) {
    def :=[X <: ~](op: UnaryOp[X]): OpAssignment[X] =
      UnaryOpAssignment(this, op)
    def apply(argument: Term[A]) = new Term[A] {
      override def evaluate[X <: ~](context: EvaluationContext[X]): ARROW[context.ROOT, X] =
        context.assignments.lookup(AbstractUnaryOp.this) o argument.evaluate(context)
    }
  }

  class AbstractBinaryOp[A: TypeTag](name: String, starTag: StarTag[A])
    extends AbstractOp[(A, A), A](
      name,
      Arity(starTag, starTag), starTag) {
    def :=[X <: ~](op: BinaryOp[X]): OpAssignment[X] =
      BinaryOpAssignment(this, op)
    def apply(left: Term[A], right: Term[A]) = new Term[A] {
      override def evaluate[X <: ~](context: EvaluationContext[X]): ARROW[context.ROOT, X] = {
        val q = context.assignments.lookup(AbstractBinaryOp.this)
        q(
          left.evaluate(context),
          right.evaluate(context)
        )
      }
    }
  }

  class AbstractRightScalarBinaryOp[A, R](name: String, starTagA: StarTag[A], starTagR: StarTag[R])
    extends AbstractOp[(A, R), A](
      name,
      Arity(starTagA, starTagR),
      starTagA
  ) {
    def :=[X <: ~, S <: ~](op: RightScalarBinaryOp[X, S]): OpAssignment[X] =
      RightScalarBinaryOpAssignment(this, op)
    def apply(left: Term[Principal], right: Term[RightScalar]) = new Term[Principal] {
      override def evaluate[X <: ~](context: EvaluationContext[X]): ARROW[context.type#ROOT, X] =
        ??? // TODO: add more machinery to make this paragraoh work
//        context.assignments.lookup(AbstractRightScalarBinaryOp.this)(
//          left.evaluate(context),
//          right.evaluate(context)
//        )
    }
  }

  trait Term[A] {
    def ::== (rightSide: Term[A]) = Equation[A](this, rightSide)
    def evaluate[X <: ~](context: EvaluationContext[X]): ARROW[context.ROOT, X]
  }
  trait Variable[A] extends Term[A]
  case class Equation[A](left: Term[A], right: Term[A])

  object Law {
    def apply[A, E](message: String, f: Variable[A] => Equation[E]): Law[Principal] =
      new Law(Arity(principal)) {}
  }

  class Law[A](arity: Arity[A]) {
    def verify[X <: ~](algebra: Algebra[X]) = {
//      val context = new RootContext(algebra, arity)
      // ... add verification stuff here
      // TDD RootContext??
    }
  }

  abstract trait EvaluationContext[X <: ~] {
    type ROOT <: ~
    val root: DOT[ROOT]
    val assignments: OpAssignments[X]
    def evaluate(term: Term[Principal]) : Quiver[ROOT, X]
  }

  abstract class RootContext[A, X <: ~](algebra: Algebra[X], arity: Arity[A]) extends EvaluationContext[X] {
    val assignments = algebra.assignments
    val variables: Array[Variable[A]]
  }

  object RootContext {
    def forNullary[X <: ~](algebra: Algebra[X]) =
      new RootContext[Unit, X](algebra, Arity()) {
        override type ROOT = UNIT
        override val root = I
        override def evaluate(term: Term[Principal]) : Quiver[ROOT, X] =
          term.evaluate(this)
        override val variables: Array[Variable[Unit]] = Array.empty
      }
    def forUnary[X <: ~](algebra: Algebra[X]) =
      new RootContext[Principal, X](algebra, Arity(principal)) {
        override type ROOT = X
        override val root = algebra.carrier
        override def evaluate(term: Term[Principal]) : Quiver[ROOT, X] =
          term.evaluate(this)
        override val variables: Array[Variable[Principal]] = Array(new Variable[Principal] {
          override def evaluate[X <: ~](context: EvaluationContext[X]): ARROW[context.type#ROOT, X] =
            ??? // TODO: fill in!
        })
      }
  }

  trait OpAssignment[X <: ~] {
    def get[A: TypeTag](abOp: AbstractNullaryOp[A]): Option[NullaryOp[X]] = None
    def get[A: TypeTag](abOp: AbstractUnaryOp[A]): Option[UnaryOp[X]] = None
    def get[A: TypeTag](abOp: AbstractBinaryOp[A]): Option[BinaryOp[X]] = None
    def get[A: TypeTag, R](abOp: AbstractRightScalarBinaryOp[A, R]): Option[RightScalarBinaryOp[X, _]] = None
  }
  case class NullaryOpAssignment[X <: ~, A: TypeTag](abstractOp: AbstractNullaryOp[A], op: NullaryOp[X]) extends OpAssignment[X] {
    override def get[B: TypeTag](abOp: AbstractNullaryOp[B]) =
      if (typeOf[B] =:= typeOf[A])
        Some(op)
      else None
  }
  case class UnaryOpAssignment[X <: ~, A: TypeTag](abstractOp: AbstractUnaryOp[A], op: UnaryOp[X]) extends OpAssignment[X] {
    override def get[B: TypeTag](abOp: AbstractUnaryOp[B]) =
      if (typeOf[B] =:= typeOf[A])
        Some(op)
      else None
  }
  case class BinaryOpAssignment[X <: ~, A: TypeTag](abstractOp: AbstractBinaryOp[A], op: BinaryOp[X]) extends OpAssignment[X] {
    override def get[B: TypeTag](abOp: AbstractBinaryOp[B]) =
      if (typeOf[B] =:= typeOf[A])
        Some(op)
      else None
  }
  case class RightScalarBinaryOpAssignment[X <: ~, S <: ~, A, R](
    abstractOp: AbstractRightScalarBinaryOp[A, R], op: RightScalarBinaryOp[X, S]) extends OpAssignment[X] {
    override def get[A: TypeTag, R](abOp: AbstractRightScalarBinaryOp[A, R]): Option[RightScalarBinaryOp[X, _]] =
        Some(op)
  }
  case class OpAssignments[X <: ~](assignments: OpAssignment[X]*) {
    // TODO: fix these up with some mechanism like A#TYPE where A = e.g. Principal??
    def lookup[A: TypeTag](op0: AbstractNullaryOp[A]): NullaryOp[X] =
      assignments.iterator.map { _.get(op0) }.collectFirst { case Some(x) => x }.get
    def lookup[A: TypeTag](op1: AbstractUnaryOp[A]): UnaryOp[X] =
      assignments.iterator.map { _.get(op1) }.collectFirst { case Some(x) => x }.get
    def lookup[A: TypeTag](op2: AbstractBinaryOp[A]): BinaryOp[X] =
      assignments.iterator.map { _.get(op2) }.collectFirst { case Some(x) => x }.get
    def lookup[A: TypeTag, R](opRSM: AbstractRightScalarBinaryOp[A, R]): RightScalarBinaryOp[X, _] =
      assignments.iterator.map { _.get(opRSM) }.collectFirst { case Some(x) => x }.get
  }

  case class AlgebraicTheory(operators: Seq[AbstractOp[_, _]], val laws: Seq[Law[_]]) {
    def apply[X <: ~](carrier: DOT[X], assignments: OpAssignment[X]*) =
      Algebra(this, carrier, OpAssignments(assignments :_*))
  }

  case class Algebra[X <: ~](theory: AlgebraicTheory, carrier: DOT[X], assignments: OpAssignments[X]) {
    def sanityTest = theory.laws.foreach { _.verify(Algebra.this)}
  }

    /*

      trait Variable extends Expression {

      }
      trait Expression {
        def evaluate[X <: ELEMENT](algebra: Algebra[X]): X
        def ::== (other: Expression) = new Equation(this, other)
      }
      class Equation(leftSide: Expression, rightSide: Expression) {
        def holds[X <: ELEMENT](algebra: Algebra[X]) =
          leftSide.evaluate(algebra) == rightSide.evaluate(algebra)
      }

      class Law(assigner: VariableAssigner, makeEquation: Array[Variable] => Equation, message: String) {
        def verify[X <: ELEMENT](algebra: Algebra[X]) = {
          val equation = makeEquation(assigner.assign(algebra))
          if (!equation.holds(algebra))
            throw new IllegalArgumentException(message)
        }
      }
      object Law {
        def leftUnit(unit: AbstractNullaryOp, multiply: AbstractBinaryOp): Law = Law("not a left unit", x =>
          multiply(unit, x) ::== x
        )
        def rightUnit(unit: AbstractNullaryOp, multiply: AbstractBinaryOp): Law = Law("not a right unit", x =>
          multiply(x, unit) ::== x
        )
        def associative(multiply: AbstractBinaryOp): Law = Law("not associative", (x, y, z) =>
          multiply(x, multiply(y, z)) ::== multiply(multiply(x, y), z)
        )

        // TODO refactor so that the function takes an array and we don't have to overload
        def apply(message: String, f: Variable => Equation): Law = {
          val assigner = new VariableAssigner(principal)
          new Law(assigner, assignments => f(assignments(0)), message)
        }
    //    def apply(message: String)(f: (Variable, Variable) => Equation): Law = ???
        def apply(message: String, f: (Variable, Variable, Variable) => Equation): Law = {
          val assigner = new VariableAssigner(principal, principal, principal)
          new Law(assigner, assignments => f(assignments(0), assignments(1), assignments(2)), message)
        }
      }

      abstract class RootContext {
        type ROOT <: ELEMENT
        val root: DOT[ROOT]
        abstract class Projection {
          type COMPONENT <: ELEMENT
          val projection: ARROW[ROOT, COMPONENT]
        }
        def projection[COMPONENT_TYPE <: ELEMENT](projectionQuiver: ARROW[ROOT, COMPONENT_TYPE]) =
          new Projection {
            override type COMPONENT = COMPONENT_TYPE
            override val projection = projectionQuiver
          }
        def variables(projections: Projection*): Array[Variable] =
          Array(new Variable {
            // wrap root.identity somehow
            override def evaluate[X <: ELEMENT](algebra: Algebra[X]) = ???
          })
      }

      class VariableAssigner(arity: StarTag*) {
        def assign[X <: ELEMENT](algebra: Algebra[X]): Array[Variable] = arity match {
          case Seq() => Array()
          case Seq(tag) =>
            import algebra.carrier
            val rootContext = new RootContext {
              override type ROOT = X
              override val root = carrier
            }
            rootContext.variables(rootContext.projection[X](carrier.identity))
          case _ => throw new IllegalArgumentException(s"Can't handle this many tags: ${arity.size}")
            // TODO: recursive function that can handle any number of them?
            // TODO: handle the tags properly (don't assume they are all principal)
        }
      }

      trait OperatorAssignment[X <: ELEMENT]
      class OperatorAssignments[X <: ELEMENT](assignments:  Seq[OperatorAssignment[X]])

      class AlgebraicTheory(operators: Set[AbstractOp], val laws: Seq[Law]) {
        def apply[X <: ELEMENT](carrier: DOT[X], assignments: OperatorAssignment[X]*) =
          Algebra(this, carrier, new OperatorAssignments(assignments))
      }

      case class Algebra[X <: ELEMENT](theory: AlgebraicTheory, carrier: DOT[X], assignments: OperatorAssignments[X]) {
        def sanityTest = theory.laws.foreach { _.verify(this)}
      }
    */
}



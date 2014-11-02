package com.fdilke.bewl.topos

import com.fdilke.bewl.topos.StarTag._

trait AlgebraicMachinery { topos: BaseTopos =>

  // Multiproducts. TODO: split out as separate trait?

  object MultiProduct {
    def apply() = new MultiProduct[UNIT] {
      val root = I
      val projections = Seq.empty
    }
  }

  abstract class MultiProduct[A <: ELEMENT] {
    val root: STAR[A]
    val projections: Seq[Quiver[A, _]]
  }

  // Multiproducts. end

  type NullaryOp[X <: ELEMENT] = QUIVER[UNIT, X]
  type Unary[X <: ELEMENT] = QUIVER[UNIT, X]
  type BinaryOp[X <: ELEMENT] = BiQuiver[X, X, X]
  type RightScalarBinaryOp[X <: ELEMENT, S <: ELEMENT] = BiQuiver[X, S, X]

  object AbstractOp {
    def unit: AbstractNullaryOp[Principal] = abstractNullaryOp("unit")
    def unitRightScalar = abstractNullaryOp("right scalar unit", rightScalar)
    def multiply = abstractBinaryOp("multiply")
    def rightScalarMultiply = new AbstractRightScalarBinaryOp("right scalar multiply")

    def abstractNullaryOp[A](name: String, starTag: StarTag[A] = principal) =
      new AbstractNullaryOp[A](name, starTag)

    def abstractBinaryOp[A](name: String, starTag: StarTag[A] = principal) =
      new AbstractBinaryOp[A](name, starTag)
  }

  case class Arity[A](tags: StarTag[_]*)
//  object Arity {
//    def apply(tag: StarTag[_], tags: StarTag[_]*) = new Arity()
//  }

//  trait Signature[RETURN_TYPE]
  class AbstractOp[A, R](name: String, arity: Arity[A], returnTag: StarTag[R])

  class AbstractNullaryOp[A](name: String, starTag: StarTag[A]) extends AbstractOp[Unit, A](name, Arity[Unit](), starTag) with Variable[A] {
    def :=[X <: ELEMENT](op: NullaryOp[X]): OpAssignment[X] =
      NullaryOpAssignment(this, op)
  }

  class AbstractBinaryOp[A](name: String, starTag: StarTag[A])
    extends AbstractOp[((Unit, A), A), A](
      name,
      Arity[((Unit, A), A)](starTag, starTag), starTag) {
    def :=[X <: ELEMENT](op: BinaryOp[X]): OpAssignment[X] =
      BinaryOpAssignment(this, op)
    def apply(left: Term[A], right: Term[A]) = new Term[A] {}
  }

  class AbstractRightScalarBinaryOp(name: String)
    extends AbstractOp[((Unit, Principal), RightScalar), Principal](
      name,
      Arity[((Unit, Principal), RightScalar)](principal, rightScalar),
      principal
  ) {
    def :=[X <: ELEMENT, S <: ELEMENT](op: RightScalarBinaryOp[X, S]): OpAssignment[X] =
      RightScalarBinaryOpAssignment(this, op)
    def apply(left: Term[Principal], right: Term[RightScalar]) = new Term[Principal] {}
  }

  trait Term[A] {
    def ::== (rightSide: Term[A]) = Equation[A](this, rightSide)
  }
  trait Variable[A] extends Term[A]
  case class Equation[A](left: Term[A], right: Term[A])

  object Law {
    def apply[A, E](message: String, f: Variable[A] => Equation[E]): Law[(Unit, Principal)] =
      new Law(Arity[(Unit, Principal)](principal)) {}
  }

  class Law[A](arity: Arity[A]) {
    def verify[X <: ELEMENT](algebra: Algebra[X]) = {
//      val context = new RootContext(algebra, arity)
      // ... add verification stuff here
      // TDD RootContext??
    }
  }

  case class RootContext[A, X <: ELEMENT](algebra: Algebra[X], arity: Arity[A]) {
    type ROOT = TRUTH // <: ELEMENT
    val root: STAR[ROOT] = omega // <: PRODUCT
    def evaluate(term: Term[Principal]) : Quiver[ROOT, X] = ???
  }

  trait OpAssignment[X <: ELEMENT]
  case class NullaryOpAssignment[X <: ELEMENT, A](abstractOp: AbstractNullaryOp[A], op: NullaryOp[X]) extends OpAssignment[X]
  case class BinaryOpAssignment[X <: ELEMENT, A](abstractOp: AbstractBinaryOp[A], op: BinaryOp[X]) extends OpAssignment[X]
  case class RightScalarBinaryOpAssignment[X <: ELEMENT, S <: ELEMENT](
    abstractOp: AbstractRightScalarBinaryOp, op: RightScalarBinaryOp[X, S]) extends OpAssignment[X]
  case class OpAssignments[X <: ELEMENT](assignments: Seq[OpAssignment[X]])

  case class AlgebraicTheory(operators: Seq[AbstractOp[_, _]], val laws: Seq[Law[_]]) {
    def apply[X <: ELEMENT](carrier: STAR[X], assignments: OpAssignment[X]*) =
      Algebra(this, carrier, OpAssignments(assignments))
  }

  case class Algebra[X <: ELEMENT](theory: AlgebraicTheory, carrier: STAR[X], assignments: OpAssignments[X]) {
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
        val root: STAR[ROOT]
        abstract class Projection {
          type COMPONENT <: ELEMENT
          val projection: QUIVER[ROOT, COMPONENT]
        }
        def projection[COMPONENT_TYPE <: ELEMENT](projectionQuiver: QUIVER[ROOT, COMPONENT_TYPE]) =
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
        def apply[X <: ELEMENT](carrier: STAR[X], assignments: OperatorAssignment[X]*) =
          Algebra(this, carrier, new OperatorAssignments(assignments))
      }

      case class Algebra[X <: ELEMENT](theory: AlgebraicTheory, carrier: STAR[X], assignments: OperatorAssignments[X]) {
        def sanityTest = theory.laws.foreach { _.verify(this)}
      }
    */
}



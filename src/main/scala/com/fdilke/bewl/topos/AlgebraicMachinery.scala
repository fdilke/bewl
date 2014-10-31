package com.fdilke.bewl.topos

trait AlgebraicMachinery { topos: BaseTopos =>

  type NullaryOp[X <: ELEMENT] = QUIVER[UNIT, X]
  type Unary[X <: ELEMENT] = QUIVER[UNIT, X]
  type BinaryOp[X <: ELEMENT] = BiQuiver[X, X, X]

  object StarTag extends Enumeration {
    type StarTag = Value
    val principal, rightMonoid = Value
  }
  import StarTag._  

/*
  abstract class AbstractOp(name: String, arity: Seq[StarTag]) {
//    type SOURCE[X <: ELEMENT]
//    def := [X <: ELEMENT](source: SOURCE[X])
  }
  class AbstractNullaryOp(name: String) extends AbstractOp(name, Seq.empty) with Expression {
    def := [X <: ELEMENT](source: NullaryOp[X]) = new OperatorAssignment[X] {}
    override def evaluate[X <: ELEMENT](algebra: Algebra[X]) = ???
  }
  class AbstractBinaryOp(name: String) extends AbstractOp(name, Seq(principal, principal)) {
    def := [X <: ELEMENT](source: BinaryOp[X]) = new OperatorAssignment[X] {}
    def apply(left: Expression, right: Expression) = new Expression {
      override def evaluate[X <: ELEMENT](algebra: Algebra[X]) = ???
    }
  }
  object AbstractOp {
    def unit = new AbstractNullaryOp("unit")
    def multiply = new AbstractBinaryOp("multiply")
  }

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

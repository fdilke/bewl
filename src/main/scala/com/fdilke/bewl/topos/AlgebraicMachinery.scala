package com.fdilke.bewl.topos

trait AlgebraicMachinery { topos: BaseTopos =>

  /*
  trait SafeArgument {
    type TYPE <: ELEMENT
    val star: STAR[TYPE]
  }

  abstract class SafeTuple[ST <: SafeTuple[ST]](args: Seq[SafeArgument]) {
    class Assignments
  }

  abstract class SafeOperator[
    X <: ELEMENT,
    ST <: SafeTuple[ST]
  ] (
    tuple: ST
  ) {
    def eval(assignments: ST#Assignments)
  }
  */

  type NullaryOp[X <: ELEMENT] = QUIVER[UNIT, X]
  type Unary[X <: ELEMENT] = QUIVER[UNIT, X]
  type BinaryOp[X <: ELEMENT] = BiQuiver[X, X, X]

  object StarTag extends Enumeration {
    type StarTag = Value
    val principal, rightMonoid = Value
  }
  import StarTag._  
  
  abstract class AbstractOp(name: String, arity: Seq[StarTag]) {
//    type SOURCE[X <: ELEMENT]
//    def := [X <: ELEMENT](source: SOURCE[X])
  }
  class AbstractNullaryOp(name: String) extends AbstractOp(name, Seq.empty) with Expression {
    def := [X <: ELEMENT](source: NullaryOp[X]) = new OperatorAssignment[X] {}
    def evaluate[X <: ELEMENT](algebra: Algebra[X]) = ???
  }
  class AbstractBinaryOp(name: String) extends AbstractOp(name, Seq(principal, principal)) {
    def := [X <: ELEMENT](source: BinaryOp[X]) = new OperatorAssignment[X] {}
    def apply(left: Expression, right: Expression) = new Expression {
      def evaluate[X <: ELEMENT](algebra: Algebra[X]) = ???
    }
  }
  object AbstractOp {
    def unit = new AbstractNullaryOp("unit")
    def multiply = new AbstractBinaryOp("multiply")
  }

  trait Variable extends Expression {

  }
  trait Expression {
    def evaluate[X <: ELEMENT](algebra: Algebra[X])
    def ::== (other: Expression) = new Equation(this, other)
  }
  class Equation(leftSide: Expression, rightSide: Expression) {
    def holds[X <: ELEMENT](algebra: Algebra[X]) =
      leftSide.evaluate(algebra) == rightSide.evaluate(algebra)
  }

  class Law(assigner: VariableAssigner, makeEquation: VariableAssignments => Equation, message: String) {
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

    def apply(message: String, f: Variable => Equation): Law = {
      val assigner = new VariableAssigner(principal)
      new Law(assigner, assignments => f(assignments.get(0)), message)
    }
//    def apply(message: String)(f: (Variable, Variable) => Equation): Law = ???
    def apply(message: String, f: (Variable, Variable, Variable) => Equation): Law = {
      val assigner = new VariableAssigner(principal, principal, principal)
      new Law(assigner, assignments => f(assignments.get(0), assignments.get(1), assignments.get(2)), message)
    }
  }

  trait VariableAssignments {
    def get(index: Int): Variable = ???
  }

  class VariableAssigner(arity: StarTag*) {
    def assign[X <: ELEMENT](algebra: Algebra[X]): VariableAssignments = ???
  }

  trait OperatorAssignment[X <: ELEMENT]

  class AlgebraicTheory(operators: Set[AbstractOp], val laws: Seq[Law]) {
    def apply[X <: ELEMENT](carrier: STAR[X], assignments: OperatorAssignment[X]*) = Algebra(this, carrier, assignments)
  }

  case class Algebra[X <: ELEMENT](theory: AlgebraicTheory, carrier: STAR[X], assignments: Seq[OperatorAssignment[X]]) {
    def sanityTest = theory.laws.foreach { _.verify(this)}
  }

//  class Algebra[X](val signature: Seq[AbstractOperator]) {}

//  object Monoids extends AlgebraicTheory {
//      signature = Seq(AbstractOp.I[X], AbstractOp.*[X, X, X]),
//      laws =
//    )
//     Algebra[X](
//  signature = Seq(AbstractOp.I[X], AbstractOp.*[X, X, X]),
//  laws =
//  )


  case class Monoid[X <: ELEMENT](carrier: STAR[X], unit: NullaryOp[X], multiply: BinaryOp[X]) {
    def sanityTest {
      // check the left unit law
      if (carrier(carrier) {
        x => multiply(unit(carrier.toI(x)), x)
      } != carrier.identity)
        throw new IllegalArgumentException("Left unit law for * with unit 1")

      // check the right unit law
      if (carrier(carrier) {
        x => multiply(x, unit(carrier.toI(x)))
      } != carrier.identity)
        throw new IllegalArgumentException("Right unit law for * with unit 1")

      // check the associative law
      if ((carrier x carrier x carrier)(carrier) {
        case ((x, y), z) => multiply(x, multiply(y, z))
      } != (carrier x carrier x carrier)(carrier) {
        case ((x, y), z) => multiply(multiply(x, y), z)
      })
        throw new IllegalArgumentException("Associative law for *")
    }

    def rightAction[A <: ELEMENT](actionCarrier: STAR[A], actionMultiply: BiQuiver[A, X, A]) =
      new RightMonoidAction[A, X](this, actionCarrier, actionMultiply)
  }

  class RightMonoidAction[A <: ELEMENT, X <: ELEMENT] (
    monoid: Monoid[X],
    actionCarrier: STAR[A],
    actionMultiply: BiQuiver[A, X, A]
  ) {
    def sanityTest = {
      // check the right unit law
      if (actionCarrier(actionCarrier) {
        a => actionMultiply(a, monoid.unit(actionCarrier.toI(a)))
      } != actionCarrier.identity)
        throw new IllegalArgumentException("Right unit law for * with unit 1")

      // check the associative law
      if ((actionCarrier x monoid.carrier x monoid.carrier)(actionCarrier) {
        case ((a, x), y) => actionMultiply(a, monoid.multiply(x, y))
      } != (actionCarrier x monoid.carrier x monoid.carrier)(actionCarrier) {
        case ((a, x), y) => actionMultiply(actionMultiply(a, x), y)
      })
        throw new IllegalArgumentException("Associative law for monoid action *")
    }
  }
}

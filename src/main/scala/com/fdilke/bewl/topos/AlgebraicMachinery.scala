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
  class AbstractNullaryOp(name: String) extends AbstractOp(name, Seq.empty) {
    def := [X <: ELEMENT](source: NullaryOp[X]) = ???
  }
  class AbstractBinaryOp(name: String) extends AbstractOp(name, Seq(principal, principal)) {
    def := [X <: ELEMENT](source: BinaryOp[X]) = ???
  }
  object AbstractOp {
    def unit = new AbstractNullaryOp("unit")
    def multiply = new AbstractBinaryOp("multiply")
  }
  
  trait Law
  object Law {
    def leftUnit(unit: AbstractNullaryOp, multiply: AbstractBinaryOp): Law = ???
    def rightUnit(unit: AbstractNullaryOp, multiply: AbstractBinaryOp): Law = ???
    def associative(multiply: AbstractBinaryOp): Law = ???
  }

  trait OperatorAssignment[X <: ELEMENT]

  class AlgebraicTheory(operators: Set[AbstractOp], laws: Seq[Law]) {
    def apply[X <: ELEMENT](carrier: STAR[X], assignments: OperatorAssignment[X]*): Algebra[X] =
      new Algebra(this, carrier, assignments)
  }

  class Algebra[X <: ELEMENT](theory: AlgebraicTheory, carrier: STAR[X], assignments: Seq[OperatorAssignment[X]]) {
    def sanityTest = ???
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

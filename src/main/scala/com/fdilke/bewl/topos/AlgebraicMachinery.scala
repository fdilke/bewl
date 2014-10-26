package com.fdilke.bewl.topos

trait AlgebraicMachinery { topos: BaseTopos =>

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

  type NullaryOp[X <: ELEMENT] = QUIVER[UNIT, X]
  type Unary[X <: ELEMENT] = QUIVER[UNIT, X]
  type BinaryOp[X <: ELEMENT] = BiQuiver[X, X, X]

  class AbstractOp
  object AbstractOp {
    def I[X <: ELEMENT]: AbstractOp = ???
    def *[L <: ELEMENT, R <: ELEMENT, T <: ELEMENT]: AbstractOp = ???
  }
  class AlgebraicTheory {}
  class Algebra[X](val signature: Seq[AbstractOp]) {}

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

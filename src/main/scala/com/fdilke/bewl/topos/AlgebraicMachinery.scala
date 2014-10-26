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
    }
  }
}

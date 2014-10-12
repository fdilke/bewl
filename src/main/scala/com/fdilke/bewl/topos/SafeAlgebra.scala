package com.fdilke.bewl.topos

trait SafeAlgebra { topos: BaseTopos =>

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
}

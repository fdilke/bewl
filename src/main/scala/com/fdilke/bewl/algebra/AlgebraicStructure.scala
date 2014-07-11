package com.fdilke.bewl.algebra

case class Operator(arity: Int)

class Law

object Law {
  def commutative(op: Operator) = new Law
}

object Operator {
  def * = new Operator(2)
}

object AlgebraicStructure {
  type Signature = Set[Operator]
}



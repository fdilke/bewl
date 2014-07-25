package com.fdilke.bewl.algebra

case class AbstractOperator(arity: Int)

object AbstractOperator {
  def * = new AbstractOperator(2)
}

object AlgebraicStructure {
  type Signature = Set[AbstractOperator]
}



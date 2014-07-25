package com.fdilke.bewl.algebra

case class AbstractOperator(arity: Int, symbol: String) {
  override def toString = symbol
}

object AbstractOperator {
  def * = new AbstractOperator(2, "*")
}

object AlgebraicStructure {
  type Signature = Set[AbstractOperator]
}



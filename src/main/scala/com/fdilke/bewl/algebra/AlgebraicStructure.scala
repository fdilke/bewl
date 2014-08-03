package com.fdilke.bewl.algebra

case class AbstractOperator(arity: Int, symbol: String) {
  override def toString = symbol
}

object AbstractOperator {
  def * = new AbstractOperator(2, "*")
  def _1 = new AbstractOperator(0, "1")
}

object AlgebraicStructure {
  type Signature = Set[AbstractOperator]

  import com.fdilke.bewl.algebra.AbstractOperator._

  def MonoidSignature = Set(_1, *)
}



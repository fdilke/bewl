package com.fdilke.bewl.algebra

import com.fdilke.bewl.Topos

case class Operator(arity: Int)

object Operator {
  def * = new Operator(2)
}

object AlgebraicStructure {
  type Signature = Set[Operator]
}



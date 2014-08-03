package com.fdilke.bewl.algebra

import com.fdilke.bewl.BaseTopos
import com.fdilke.bewl.algebra.AlgebraicStructure._

trait AlgebraicStructures { topos: BaseTopos with Algebra =>
  case class Monoid[X](dot: DOT[X], unit: Operator[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    signature = MonoidSignature,
    operatorMap = Map(AbstractOperator._1 -> unit, AbstractOperator.* -> product),
    Law.unit(AbstractOperator._1, AbstractOperator.*),
    Law.associative(AbstractOperator.*)
  )
}




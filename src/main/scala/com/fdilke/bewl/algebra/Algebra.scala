package com.fdilke.bewl.algebra

import com.fdilke.bewl.BaseTopos

// Machinery for constructing and verifying algebraic structures with laws (varieties)

case class AbstractOperator(arity: Int, symbol: String) {
  override def toString = symbol
}

object AbstractOperator {
  def * = new AbstractOperator(2, "*")
  def ++ = new AbstractOperator(2, "+")
  def _1 = new AbstractOperator(0, "1")
  def _0 = new AbstractOperator(0, "0")
  def -- = new AbstractOperator(1, "negate")
  def invert = new AbstractOperator(1, "invert")
  def ^ = new AbstractOperator(2, "^")
  def v = new AbstractOperator(2, "v")
  def > = new AbstractOperator(2, "imply")
}

trait Algebra {
  topos: BaseTopos =>

  class Law(abstractOperators: Seq[AbstractOperator],
            numVariables: Int,
            _equation: PartialFunction[(Seq[BoundAlgebraicOperator[Power[Nothing], Nothing]], Seq[Operator[Nothing]]), Boolean],
            exceptionMessageGenerator: PartialFunction[Seq[AbstractOperator], String]
            ) {

    def remap(mappings: Map[AbstractOperator, AbstractOperator]) = new Law(
      abstractOperators.map { aop =>
        mappings.get(aop).getOrElse(aop)
      },
      numVariables, _equation, exceptionMessageGenerator
    )

    def checkCoveredBy(signature: Seq[AbstractOperator]) =
      for (abstractOperator <- abstractOperators) {
        if (!signature.contains(abstractOperator)) {
          throw new IllegalArgumentException(s"Law uses undefined operator $abstractOperator")
        }
      }

    // TODO: can just assign from _equation with appropriate type hints?
    def equation[X]: PartialFunction[(Seq[BoundAlgebraicOperator[Power[X], X]], Seq[Operator[X]]), Boolean] =
      _equation.asInstanceOf[PartialFunction[(Seq[BoundAlgebraicOperator[Power[X], X]], Seq[Operator[X]]), Boolean]]

    def verify[X](carrier: DOT[X], operatorMap: Map[AbstractOperator, Operator[X]]) = {
      val power = carrier A numVariables
      val variables = power.projection
      val operators = abstractOperators map { abstractOp =>
        BoundAlgebraicOperator[Power[X], X](power.power, operatorMap(abstractOp))
      }

      val operatorsAndVariables = (operators, variables)
      if (!equation[X](operatorsAndVariables)) {
        throw new IllegalArgumentException(exceptionMessageGenerator(abstractOperators))
      }
    }
  }

  case class BoundAlgebraicOperator[S, X](val source: DOT[S], arrow: Operator[X]) {
    def apply(variables: ARROW[S, X]*): ARROW[S, X] =
      arrow(IntegerPower.multiply(source, variables: _*))
  }

}

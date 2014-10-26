package com.fdilke.bewl.diagrammatic.algebra

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos

// Machinery for constructing and verifying algebraic structures with laws (varieties)

case class DiagrammaticAbstractOperator(arity: Int, symbol: String) {
  override def toString = symbol
}

object DiagrammaticAbstractOperator {
  def * = new DiagrammaticAbstractOperator(2, "*")
  def ++ = new DiagrammaticAbstractOperator(2, "+")
  def _1 = new DiagrammaticAbstractOperator(0, "1")
  def _0 = new DiagrammaticAbstractOperator(0, "0")
  def -- = new DiagrammaticAbstractOperator(1, "negate")
  def invert = new DiagrammaticAbstractOperator(1, "invert")
  def ^ = new DiagrammaticAbstractOperator(2, "^")
  def v = new DiagrammaticAbstractOperator(2, "v")
  def > = new DiagrammaticAbstractOperator(2, "imply")
}

trait DiagrammaticAlgebra {
  topos: BaseDiagrammaticTopos =>

  class Law(abstractOperators: Seq[DiagrammaticAbstractOperator],
            numVariables: Int,
            _equation: PartialFunction[(Seq[BoundDiagrammaticAlgebraicOperator[Power[Nothing], Nothing]], Seq[Operator[Nothing]]), Boolean],
            exceptionMessageGenerator: PartialFunction[Seq[DiagrammaticAbstractOperator], String]
            ) {

    def remap(mappings: Map[DiagrammaticAbstractOperator, DiagrammaticAbstractOperator]) = new Law(
      abstractOperators.map { aop =>
        mappings.get(aop).getOrElse(aop)
      },
      numVariables, _equation, exceptionMessageGenerator
    )

    def checkCoveredBy(signature: Seq[DiagrammaticAbstractOperator]) =
      for (abstractOperator <- abstractOperators)
        if (!signature.contains(abstractOperator))
          throw new IllegalArgumentException(s"Law uses undefined operator $abstractOperator")

    // TODO: can just assign from _equation with appropriate type hints?
    def equation[X]: PartialFunction[(Seq[BoundDiagrammaticAlgebraicOperator[Power[X], X]], Seq[Operator[X]]), Boolean] =
      _equation.asInstanceOf[PartialFunction[(Seq[BoundDiagrammaticAlgebraicOperator[Power[X], X]], Seq[Operator[X]]), Boolean]]

    def verify[X](carrier: DOT[X], operatorMap: Map[DiagrammaticAbstractOperator, Operator[X]]) = {
      val power = carrier A numVariables
      val variables = power.projection
      val operators = abstractOperators map { abstractOp =>
        BoundDiagrammaticAlgebraicOperator[Power[X], X](power.power, operatorMap(abstractOp))
      }

      val operatorsAndVariables = (operators, variables)
      if (!equation[X](operatorsAndVariables)) {
        throw new IllegalArgumentException(exceptionMessageGenerator(abstractOperators))
      }
    }
  }

  case class BoundDiagrammaticAlgebraicOperator[S, X](val source: DOT[S], arrow: Operator[X]) {
    def apply(variables: ARROW[S, X]*): ARROW[S, X] =
      arrow(IntegerPower.multiply(source, variables: _*))
  }
}

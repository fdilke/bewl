package com.fdilke.bewl.algebra

import com.fdilke.bewl.BaseTopos
import com.fdilke.bewl.algebra.AlgebraicStructure._

// Machinery for constructing and verifying algebraic structures with laws (varieties)

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

trait Algebra {
  topos: BaseTopos =>

  class Law(
             abstractOperators: Seq[AbstractOperator],
             numVariables: Int,
             _equation: PartialFunction[(Seq[BoundAlgebraicOperator[Power[Nothing], Nothing]], Seq[Operator[Nothing]]), Boolean],
             exceptionMessage: String
             ) {

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
        throw new IllegalArgumentException(exceptionMessage)
      }
    }
  }

  object Law {
    def commutative(aop: AbstractOperator) =
      new Law(Seq(aop), 2, { case (Seq(op), Seq(x, y)) =>
        op(x, y) == op(y, x)
      }, s"Commutative law for operator $aop"
      )

    def associative(aop: AbstractOperator) =
      new Law(Seq(aop), 3, { case (Seq(op), Seq(x, y, z)) =>
        op(x, op(y, z)) == op(op(x, y), z)
      },
      s"Associative law for operator $aop"
      )

    def unit(abstractUnit: AbstractOperator, aop: AbstractOperator) =
      new Law(Seq(abstractUnit, aop), 1, { case (Seq(unit, op), Seq(x)) =>
        val u = unit()
        op(u, x) == x && op(x, u) == x
      },
      s"Unit law for operator $aop with unit $abstractUnit"
      )
  }

  case class BoundAlgebraicOperator[S, X](val source: DOT[S], arrow: Operator[X]) {
    def apply(variables: ARROW[S, X]*): ARROW[S, X] =
      arrow(IntegerPower.multiply(source, variables: _*))
  }

  class AlgebraicStructure[X](
                               val carrier: DOT[X],
                               val signature: Signature,
                               val operatorMap: Map[AbstractOperator, Operator[X]],
                               val laws: Law*) {
    def verify = laws.map {
      _.verify(carrier, operatorMap)
    }
  }
}

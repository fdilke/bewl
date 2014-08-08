package com.fdilke.bewl.algebra

import com.fdilke.bewl.BaseTopos

trait AlgebraicStructures { topos: BaseTopos with Algebra =>
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

    def leftUnit(abstractUnit: AbstractOperator, aop: AbstractOperator) =
      new Law(Seq(abstractUnit, aop), 1, { case (Seq(unit, op), Seq(x)) =>
          val u = unit()
          op(u, x) == x
        },
        s"Left unit law for operator $aop with unit $abstractUnit"
      )

    def rightUnit(abstractUnit: AbstractOperator, aop: AbstractOperator) =
      new Law(Seq(abstractUnit, aop), 1, { case (Seq(unit, op), Seq(x)) =>
          val u = unit()
          op(x, u) == x
        },
        s"Right unit law for operator $aop with unit $abstractUnit"
      )

    def leftInverse(abstractUnit: AbstractOperator, abstractInverse: AbstractOperator, aop: AbstractOperator) =
      new Law(Seq(abstractUnit, abstractInverse, aop), 1, { case (Seq(unit, inv, op), Seq(x)) =>
          val u = unit()
          op(inv(x), x) == u
        },
        s"Left inverse law for operator $aop with inverse $abstractInverse and unit $abstractUnit"
      )

    def rightInverse(abstractUnit: AbstractOperator, abstractInverse: AbstractOperator, aop: AbstractOperator) =
      new Law(Seq(abstractUnit, abstractInverse, aop), 1, { case (Seq(unit, inv, op), Seq(x)) =>
        val u = unit()
        op(x, inv(x)) == u
      },
      s"Right inverse law for operator $aop with inverse $abstractInverse and unit $abstractUnit"
      )

    def leftDistributive(abstractProduct: AbstractOperator, abstractSum: AbstractOperator) =
      new Law(Seq(abstractProduct, abstractSum), 3, { case (Seq(prd, sum), Seq(x, y, z)) =>
        prd(x, sum(y, z)) == sum(prd(x, y), prd(x, z))
      },
      s"Left distributive law for operator $abstractProduct over $abstractSum"
      )

    def rightDistributive(abstractProduct: AbstractOperator, abstractSum: AbstractOperator) =
      new Law(Seq(abstractProduct, abstractSum), 3, { case (Seq(prd, sum), Seq(x, y, z)) =>
        prd(sum(x, y), z) == sum(prd(x, z), prd(y, z))
      },
      s"Right distributive law for operator $abstractProduct over $abstractSum"
      )
  }

  import Law._
  import AbstractOperator._

  case class Monoid[X](dot: DOT[X], unit: Operator[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_1 -> unit, * -> product),
    leftUnit(_1, *),
    rightUnit(_1, *),
    associative(*)
  )

  case class Group[X](dot: DOT[X],
                      unit: Operator[X],
                      product: Operator[X],
                      inversion: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_1 -> unit,
                      * -> product,
                      invert -> inversion),
    leftUnit(_1, *),
    rightUnit(_1, *),
    associative(*),
    leftInverse(_1, invert, *),
    rightInverse(_1, invert, *)
  )

  case class AbelianGroup[X](dot: DOT[X],
                      zero: Operator[X],
                      sum: Operator[X],
                      negate: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_0 -> zero,
                      ++ -> sum,
                      -- -> negate),
    leftUnit(_0, ++),
    rightUnit(_0, ++),
    associative(++),
    leftInverse(_0, --, ++),
    rightInverse(_0, --, ++),
    commutative(++)
  )

  case class Ring[X](dot: DOT[X],
                     zero: Operator[X],
                     one: Operator[X],
                     sum: Operator[X],
                     negate: Operator[X],
                     product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_0 -> zero,
                      _1 -> one,
                      ++ -> sum,
                      -- -> negate,
                      * -> product),
    leftUnit(_0, ++),
    rightUnit(_0, ++),
    associative(++),
    leftInverse(_0, --, ++),
    rightInverse(_0, --, ++),
    commutative(++),
    associative(*),
    leftUnit(_1, *),
    rightUnit(_1, *),
    leftDistributive(*, ++),
    rightDistributive(*, ++)
  )
}




package com.fdilke.bewl.diagrammatic.algebra

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos

trait DiagrammaticAlgebraicLaws {
  topos: BaseDiagrammaticTopos with DiagrammaticAlgebra =>

  def commutative(aop: DiagrammaticAbstractOperator) =
    new Law(Seq(aop), 2, { case (Seq(op), Seq(x, y)) =>
      op(x, y) == op(y, x)
    }, { case Seq(anop) => s"Commutative law for $anop" }
    )

  def associative(aop: DiagrammaticAbstractOperator) =
    new Law(Seq(aop), 3, { case (Seq(op), Seq(x, y, z)) =>
      op(x, op(y, z)) == op(op(x, y), z)
    },
    { case Seq(anop) => s"Associative law for $anop" }
    )

  def leftUnit(abstractUnit: DiagrammaticAbstractOperator, aop: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractUnit, aop), 1, { case (Seq(unit, op), Seq(x)) =>
      val u = unit()
      op(u, x) == x
    },
    { case Seq(anopUnit, anop) => s"Left unit law for $anop with unit $anopUnit" }
    )

  def rightUnit(abstractUnit: DiagrammaticAbstractOperator, aop: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractUnit, aop), 1, { case (Seq(unit, op), Seq(x)) =>
      val u = unit()
      op(x, u) == x
    },
    { case Seq(anopUnit, anop) => s"Right unit law for $anop with unit $anopUnit" }
    )

  def leftInverse(abstractUnit: DiagrammaticAbstractOperator, abstractInverse: DiagrammaticAbstractOperator, aop: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractUnit, abstractInverse, aop), 1, { case (Seq(unit, inv, op), Seq(x)) =>
      val u = unit()
      op(inv(x), x) == u
    },
    { case Seq(anopUnit, anopInv, anop) => s"Left inverse law for $anop with inverse $anopInv and unit $anopUnit" }
    )

  def rightInverse(abstractUnit: DiagrammaticAbstractOperator, abstractInverse: DiagrammaticAbstractOperator, aop: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractUnit, abstractInverse, aop), 1, { case (Seq(unit, inv, op), Seq(x)) =>
      val u = unit()
      op(x, inv(x)) == u
    },
    { case Seq(anopUnit, anopInv, anop) => s"Right inverse law for $anop with inverse $anopInv and unit $anopUnit" }
    )

  def leftDistributive(abstractProduct: DiagrammaticAbstractOperator, abstractSum: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractProduct, abstractSum), 3, { case (Seq(prd, sum), Seq(x, y, z)) =>
      prd(x, sum(y, z)) == sum(prd(x, y), prd(x, z))
    },
    { case Seq(anopProduct, anopSum) => s"Left distributive law for $anopProduct over $anopSum" }
    )

  def rightDistributive(abstractProduct: DiagrammaticAbstractOperator, abstractSum: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractProduct, abstractSum), 3, { case (Seq(prd, sum), Seq(x, y, z)) =>
      prd(sum(x, y), z) == sum(prd(x, z), prd(y, z))
    },
    { case Seq(anopProduct, anopSum) => s"Right distributive law for $anopProduct over $anopSum" }
    )

  def absorptive(abstractOver: DiagrammaticAbstractOperator, abstractUnder: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractOver, abstractUnder), 2, { case (Seq(over, under), Seq(x, y)) =>
      over(x, under(x, y)) == x
    },
    { case Seq(anopOver, anopUnder) => s"Absorptive law for $anopOver over $anopUnder" }
    )

  def selfImplication(abstractTruth: DiagrammaticAbstractOperator, abstractImply: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractTruth, abstractImply), 1, { case (Seq(truth, imply), Seq(x)) =>
      truth() == imply(x, x)
    },
    { case Seq(anopTruth, anopImplies) => s"Self implication law for $anopTruth and $anopImplies" }
    )

  def modusPonens(abstractAnd: DiagrammaticAbstractOperator, abstractImply: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractAnd, abstractImply), 2, { case (Seq(and, imply), Seq(x, y)) =>
      and(x, imply(x, y)) == and(x, y)
    },
    { case Seq(anopAnd, anopImplies) => s"Modus ponens for $anopAnd and $anopImplies" }
    )

  def implicationSupersedes(abstractAnd: DiagrammaticAbstractOperator, abstractImply: DiagrammaticAbstractOperator) =
    new Law(Seq(abstractAnd, abstractImply), 2, { case (Seq(and, imply), Seq(x, y)) =>
      and(x, imply(y, x)) == x
    },
    { case Seq(anopAnd, anopImplies) => s"Implication supersedes for $anopAnd and $anopImplies" }
    )
}

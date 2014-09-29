package com.fdilke.bewl.diagrammatic.algebra

import AbstractOperator._
import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos

trait AlgebraicStructures { topos: BaseDiagrammaticTopos with Algebra with AlgebraicLaws =>
  class AlgebraicTheory(signature: Seq[AbstractOperator], laws: Law*) {

    // Make sure all the operators used in the laws are defined
    for(law <- laws) {
      law.checkCoveredBy(signature)
    }

    def verify[X](carrier: DOT[X], operatorMap: Map[AbstractOperator, Operator[X]]) = laws.map {
      _.verify(carrier, operatorMap)
    }

    def extend(moreLaws: Law*): AlgebraicTheory =
      extend(Seq(), moreLaws :_*)

    def extend(moreOperators: Seq[AbstractOperator], moreLaws: Law*) =
      new AlgebraicTheory(signature ++ moreOperators, (laws ++ moreLaws):_*)

    def remap(mappings: (AbstractOperator, AbstractOperator)*) = {
      val mappingsMap = Map(mappings :_*)
      val remappedSignature = signature.map { aop =>
        mappingsMap.get(aop).getOrElse(aop)
      }
      val remappedLaws = laws.map { _.remap(mappingsMap)}
      new AlgebraicTheory(remappedSignature, remappedLaws :_*)
    }
  }

  class AlgebraicStructure[X](val carrier: DOT[X],
    val operatorMap: Map[AbstractOperator, Operator[X]],
    val theory: AlgebraicTheory) {
    def verify = theory.verify(carrier, operatorMap)
  }

  def Monoids = new AlgebraicTheory(Seq(_1, *),
        leftUnit(_1, *),
        rightUnit(_1, *),
        associative(*)
      )

  def Groups = Monoids.extend(Seq(invert),
    leftInverse(_1, invert, *),
    rightInverse(_1, invert, *)
  )

  def AdditiveAbelianGroups = Groups.remap(_1 -> _0, * -> ++, invert -> --).extend(
    commutative(++)
  )

  def Rings = AdditiveAbelianGroups.extend(Seq(_1, *),
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

  def Lattices = new AlgebraicTheory(Seq(_0, _1, v, ^),
    leftUnit(_0, v),
    rightUnit(_0, v),
    commutative(v),
    associative(v),

    leftUnit(_1, ^),
    rightUnit(_1, ^),
    commutative(^),
    associative(^),

    absorptive(^, v),
    absorptive(v, ^)
  )

  def HeytingAlgebras = Lattices.extend(Seq(>),
    selfImplication(_1, >),
    modusPonens(^, >),
    implicationSupersedes(^, >),
    leftDistributive(>, ^)
  )

  case class Monoid[X](dot: DOT[X], unit: Operator[X], product: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_1 -> unit, * -> product),
    theory = Monoids
  )

  case class Group[X](dot: DOT[X],
                      unit: Operator[X],
                      product: Operator[X],
                      inversion: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_1 -> unit,
                      * -> product,
                      invert -> inversion),
    theory = Groups
  )

  case class AdditiveAbelianGroup[X](dot: DOT[X],
                      zero: Operator[X],
                      sum: Operator[X],
                      negate: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_0 -> zero,
                      ++ -> sum,
                      -- -> negate),
    theory = AdditiveAbelianGroups
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
    theory = Rings
  )

  case class Lattice[X](dot: DOT[X],
                     zero: Operator[X],
                     one: Operator[X],
                     meet: Operator[X],
                     join: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_0 -> zero,
                      _1 -> one,
                      ^ -> meet,
                      v -> join
    ),
    theory = Lattices
  )

  case class HeytingAlgebra[X](dot: DOT[X],
                     zero: Operator[X],
                     one: Operator[X],
                     meet: Operator[X],
                     join: Operator[X],
                     implies: Operator[X]) extends AlgebraicStructure[X] (
    carrier = dot,
    operatorMap = Map(_0 -> zero,
                      _1 -> one,
                      ^ -> meet,
                      v -> join,
                      > -> implies
    ),
    theory = HeytingAlgebras
  )
}




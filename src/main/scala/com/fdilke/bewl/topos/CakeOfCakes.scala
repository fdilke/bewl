package com.fdilke.bewl.topos

import com.fdilke.bewl.topos.algebra.{AlgebraicStructures, AlgebraicConstructions, AlgebraicMachinery}
import com.fdilke.bewl.topos.constructions.{ConstructToposOfAutomorphisms, ConstructToposOfGroupActions, ConstructToposOfMonoidActions}

trait ToposAlgebra[~] extends
  AlgebraicMachinery[~] with
  AlgebraicConstructions[~] with
  AlgebraicStructures[~]

trait ToposConstructions[~] extends BaseTopos[~]
  with ConstructToposOfMonoidActions[~]
  with ConstructToposOfGroupActions[~]
  with ConstructToposOfAutomorphisms[~] {

  Ɛ: AlgebraicStructures[~] with
    AlgebraicMachinery[~] with
    LogicalOperations[~] with
    Monads[~] =>
}

trait Topos[~] extends BaseTopos[~] with
  Monads[~] with
  LogicalOperations[~] with
  ToposAlgebra[~] with
  ToposConstructions[~]

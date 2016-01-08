package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.topos.{Topos, Wrappings, BaseTopos, LogicalOperations}

trait ConstructToposOfGroupActions extends BaseTopos with LogicalOperations {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

//  object ToposOfGroupActions {
//    def of[G <: ~](group: Ɛ.Group[G]) : Topos with Wrappings[
//      Ɛ.~,
//      ({type λ[X <: Ɛ.~] = group.Action[X]})#λ,
//      ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = group.ActionPreArrow[X, Y]})#λ
//    ] = {
//
//  }
}

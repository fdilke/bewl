package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.topos.{BaseTopos, LogicalOperations}

trait ConstructToposOfGroupActions extends BaseTopos with LogicalOperations {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

//  object ToposOfGroupActions {
//    def of[G <: ~](group: Ɛ.Group[G]) : Topos with Wrappings[
//      Ɛ.~,
//      ({type λ[X <: Ɛ.~] = monoid.Action[X]})#λ,
//      ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = monoid.ActionPreArrow[X, Y]})#λ
//    ] = {
//
//  }
}

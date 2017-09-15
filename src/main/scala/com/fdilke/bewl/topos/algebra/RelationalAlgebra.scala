package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos.{BaseTopos, ToposEnrichments, ToposStructures}

import scala.language.higherKinds

trait RelationalAlgebra extends
  BaseTopos with
  ToposEnrichments with
  ToposStructures with
  AlgebraicMachinery { builder: AlgebraicStructures =>

}

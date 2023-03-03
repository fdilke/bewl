package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.ProductMappable
import com.fdilke.utility.Shortcuts.*

import scala.language.{dynamics, postfixOps}
import ProductMappable._

sealed trait AlgebraicSort
class Principal extends AlgebraicSort
class Scalar extends AlgebraicSort

trait AlgebraicMachinery[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicTheories[DOT, CTXT, VOID, UNIT, BEWL, >]
  with AlgebraicStructures[DOT, CTXT, VOID, UNIT, BEWL, >]
  with AlgebraicConstructions[DOT, CTXT, VOID, UNIT, BEWL, >]:
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>




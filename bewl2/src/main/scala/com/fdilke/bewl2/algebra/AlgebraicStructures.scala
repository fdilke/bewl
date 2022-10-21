package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.Topos
import com.fdilke.utility.Shortcuts.*

import scala.annotation.targetName
import scala.language.{dynamics, postfixOps}
import Mappable._

trait AlgebraicStructures[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  →[_, _]
] extends AlgebraicTheories[DOT, CTXT, VOID, UNIT, BEWL, →] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, →] =>
}

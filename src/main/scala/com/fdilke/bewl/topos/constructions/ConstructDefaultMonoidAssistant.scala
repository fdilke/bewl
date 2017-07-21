package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos._
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import scala.language.higherKinds
import scala.language.reflectiveCalls

trait ConstructDefaultMonoidAssistant extends
  BaseTopos with
  ToposEnrichments {

  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  // TODO fix 'bakery of doom' issue and move the 
  // monoid assistant code in here

  
}
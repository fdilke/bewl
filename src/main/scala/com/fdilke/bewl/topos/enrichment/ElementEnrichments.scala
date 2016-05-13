package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}
import scala.language.implicitConversions

trait ElementEnrichments {

  Ɛ: BaseTopos with ToposStructures =>

  class PoorExponential[
    S <: ~,
    T <: ~
  ] (
    exp: S → T
  ) (
    implicit expDot: EXPONENTIAL[S, T]
  ) {
    def notThisName [
      R <: ~
    ] (
      preExp: R → S
    ) (
      implicit preExpDot: EXPONENTIAL[R, S]
    ): R → T = (
        preExpDot.source > expDot.target
      ).transpose (
        preExpDot
      ) {
        (sr, r) => exp(sr(r))
      } (
        preExp
      )
  }

//  implicit def enrichExp[
//    S <: ~,
//    T <: ~
//  ] (
//    exp: S → T
//  ) (
//    implicit preExpDot: EXPONENTIAL[S, T]
//  ): RichExponential[S, T] =
//    new RichExponential(exp)(preExpDot)

}

package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}
import scala.language.implicitConversions

trait ElementEnrichments {

  Ɛ: BaseTopos with ToposStructures =>

  implicit class RichExponential[
    S <: ~,
    T <: ~
  ](
    exp: S → T
  )(
    implicit expDot: EXPONENTIAL[S, T]
  ) {
    def o[
      R <: ~
    ](
      preExp: R → S
    )(
      implicit preExpDot: EXPONENTIAL[R, S]
    ): R → T =
      (
        preExpDot.source > expDot.target
      ).transpose(
        preExpDot
      )((sr, r) => exp(sr(r)))(
        preExp
      )

    def o[
      R <: ~
    ](
      preExp: R > S
    ): R → T =
      (expDot.target > preExp)(
        exp
      )
  }
}

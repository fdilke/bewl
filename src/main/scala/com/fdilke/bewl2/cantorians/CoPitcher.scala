package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Compact.{forAll, optional}
import com.fdilke.bewl2.topology.{Compact, Hausdorff}

object CoPitcher {

  def isConstant[
    P,
    C: Compact,
    H: Hausdorff
  ](
    fn: P => H
  )(
    implicit pitcherTude: Pitcher[P, C]
  ): Boolean =
    optional[C] match {
      case None =>
        throw new IllegalArgumentException("Domain is empty")
      case Some(sampleC) =>
        val candidate: H = fn(
          Pitcher.constantly[P, C](sampleC)
        )
        forAll[P] { p =>
          Hausdorff.equalH(
            fn(p),
            candidate
          )
        }(
          Pitcher.compactness[P, C]
        )
    }

}

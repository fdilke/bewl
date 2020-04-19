package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.topology.Compact.{forAll, optional}
import com.fdilke.bewl2.topology.{Compact, Hausdorff}

object CoPitcher {

  def detectConstant[
    P,
    C: Compact,
    H: Hausdorff
  ](
    coP: P => H
  )(
    implicit pitcherTude: Pitcher[P, C]
  ): Option[H] =
    optional[C] match {
      case None =>
        throw new IllegalArgumentException("Domain is empty")
      case Some(sampleC) =>
        val candidate: H = coP(
          Pitcher.constantly[P, C](sampleC)
        )
        if (forAll[P] { p =>
              Hausdorff.equalH(
                coP(p),
                candidate
              )
            }(
              Pitcher.compactness[P, C]
            ))
          Some(candidate)
        else
          None
    }

  def functionAsCatcher[
    P,
    C: Compact,
    H: Hausdorff
  ](
    implicit pitcherTude: Pitcher[P, C]
  ): Catcher[P => H, C, H] =
    new Catcher[P => H, C, H] {
      override def either(
        coP: P => H
      ): Either[H, C => P => H] =
        detectConstant(coP) match {
          case Some(h) => Left(h)
          case None =>
            Right { c => p =>
              coP(
                pitcherTude.construct(c, p)
              )
            }
        }

      override def construct(e: => Either[H, C => P => H]): P => H =
        ???
    }
}

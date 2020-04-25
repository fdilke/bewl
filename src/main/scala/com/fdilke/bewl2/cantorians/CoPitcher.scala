package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Pitcher.{head, tail}
import com.fdilke.bewl2.topology.Compact.{forAll, optional}
import com.fdilke.bewl2.topology.{Compact, Hausdorff}
//import CoPitcher._

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
          case Some(h) =>
            Left(h)
          case None =>
            Right { c => p =>
              coP(
                Pitcher.construct(c, p)
              )
            }
        }

      override def construct(
        e: => Either[H, C => P => H]
      ): P => H =
        e match {
          case Left(h) =>
            _ => h
          case Right(c2p2h) =>
            p => c2p2h(head(p))(tail(p))
        }
    }

  implicit def catcherTude[P, C: Compact, H: Hausdorff](
    implicit pitcherTude: Pitcher[P, C]
  ): Catcher[CoPitcher[P, C, H], C, H] =
    new Catcher[CoPitcher[P, C, H], C, H] {
      override def either(
        coP: CoPitcher[P, C, H]
      ): Either[H, C => CoPitcher[P, C, H]] =
        detectConstant(coP.function) match {
          case Some(h) =>
            Left(h)
          case None =>
            Right { c =>
              new CoPitcher[P, C, H](p =>
                coP.function(
                  Pitcher.construct(c, p)
                )
              )
            }
        }

      override def construct(
        e: => Either[H, C => CoPitcher[P, C, H]]
      ): CoPitcher[P, C, H] =
        new CoPitcher[P, C, H](
          e match {
            case Left(h) =>
              _ => h
            case Right(c2p2h) =>
              p => c2p2h(head(p)).function(tail(p))
          }
        )
    }
}

class CoPitcher[P, C: Compact, H: Hausdorff](
  val function: P => H
)(
  implicit pitcherTude: Pitcher[P, C]
) { coPitcher =>
  def recastAs[CT](
    implicit ctCatcherTude: Catcher[CT, C, H]
  ): CT =
    Catcher.recast[CoPitcher[P, C, H], CT, C, H](
      coPitcher
    )
}

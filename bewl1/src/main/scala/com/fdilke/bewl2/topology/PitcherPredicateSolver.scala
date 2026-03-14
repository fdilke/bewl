package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.Pitcher
import com.fdilke.bewl2.cantorians.Pitcher._
import com.fdilke.bewl2.topology.Compact._

import scala.annotation.tailrec
import scala.language.postfixOps

object PitcherPredicateSolver {
  def solveSeq[
    P,
    C: Compact
  ](
    predicate: P => Boolean
  )(
    implicit pitcher: Pitcher[P, C]
  ): Option[Seq[C]] =
    new PitcherPredicateSolver(
      predicate
    )(Compact[C], pitcher)
      .trySeq { // <== should not be needed
        DraftPitcher.empty
      }
      .map {
        _.seq
      }

  def solvePitcher[
    P,
    C: Compact
  ](
    predicate: P => Boolean
  )(
    implicit pitcher: Pitcher[P, C]
  ): Option[P] =
    optional[C].flatMap { c =>
      solveSeq(
        predicate
      )(Compact[C], pitcher).map { seq => // <== should not be needed
        GoPlatinumPitcher(seq, c)
      }
    }
}

class PitcherPredicateSolver[
  P,
  C: Compact
](
  predicate: P => Boolean
)(
  implicit pitcher: Pitcher[P, C]
) {
  def trySeq(
    draft: DraftPitcher[P, C]
  ): Option[DraftPitcher[P, C]] =
    (try {
      Left(
        if (predicate(
              draft.asPitcher
            ))
          Some(draft)
        else
          None
      )
    } catch {
      case StumpedException => Right(())
    }) match {
      case Left(result) => result
      case Right(_) =>
        determine[C, Option[DraftPitcher[P, C]]](
          c => trySeq(draft.plus(c)),
          od => od.isDefined
        ).flatMap {
          _._2
        }
    }
}

case object StumpedException extends Exception

object DraftPitcher {
  def empty[C: Compact, P]: DraftPitcher[P, C] =
    new DraftPitcher[P, C](Seq.empty)
}

class DraftPitcher[P, C](
  val seq: Seq[C]
) extends AnyVal {
  def asPitcher(
    implicit pitcher: Pitcher[P, C]
  ): P = {
    @inline def conditionally[X](
      block: => X
    ): X =
      if (seq.isEmpty)
        throw StumpedException
      else
        block

    construct(
      conditionally {
        seq head
      },
      conditionally {
        new DraftPitcher[P, C](
          seq tail
        ) asPitcher
      }
    )
  }

  def plus(c: C): DraftPitcher[P, C] =
    new DraftPitcher[P, C](seq :+ c)
}

object GoPlatinumPitcher {
  def apply[P, C](
    seq: Seq[C],
    backstop: C
  )(
    implicit pitcher: Pitcher[P, C]
  ): P =
    seq.foldRight[P](
      constantly(backstop)
    )((c: C, p: P) => construct[P, C](c, p))
}

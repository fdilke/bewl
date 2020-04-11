package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.{Pitcher}
import com.fdilke.bewl2.topology.Compact._
import Pitcher._
import scala.annotation.tailrec
import scala.language.postfixOps

object PitcherPredicateSolver {
  def solveSeq[
    C: Compact,
    P[_]: Pitcher
  ](
    predicate: P[C] => Boolean
  ): Option[Seq[C]] =
    new PitcherPredicateSolver(
      predicate
    ) trySeq {
      DraftPitcher.empty
    } map {
      _.seq
    }

  def solvePitcher[
    C: Compact,
    P[_]: Pitcher
  ](
    predicate: P[C] => Boolean
  ): Option[P[C]] =
    Compact[C].optional flatMap { c =>
      solveSeq(predicate) map { seq => GoPlatinumPitcher(seq, c) }
    }
}

class PitcherPredicateSolver[
  C: Compact,
  P[_]: Pitcher
](
  predicate: P[C] => Boolean
) {
  @tailrec private final def trySeq(
    draft: DraftPitcher[C, P]
  ): Option[DraftPitcher[C, P]] =
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
        determine[C] { c =>
          trySeqNonTailRec(
            draft.plus(c)
          ) isDefined
        } match { // exercise for the student, why can't this be a flatmap?
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            trySeq(draft.plus(c))
          case None => None
        }
    }

  private def trySeqNonTailRec(
    draft: DraftPitcher[C, P]
  ): Option[DraftPitcher[C, P]] =
    trySeq(draft)
}

case object StumpedException extends Exception

object DraftPitcher {
  def empty[C: Compact, P[_]]: DraftPitcher[C, P] =
    new DraftPitcher[C, P](Seq.empty)
}

class DraftPitcher[C, P[_]](
  val seq: Seq[C]
) extends AnyVal {
  def asPitcher(
    implicit pitcher: Pitcher[P]
  ): P[C] = {
    @inline def conditionally[X](
      block: => X
    ): X =
      if (seq.isEmpty)
        throw StumpedException
      else
        block

    Pitcher[P].construct(
      conditionally {
        seq head
      },
      conditionally {
        new DraftPitcher(
          seq tail
        ) asPitcher
      }
    )
  }

  def plus(c: C): DraftPitcher[C, P] =
    new DraftPitcher[C, P](seq :+ c)
}

object GoPlatinumPitcher {
  def apply[C, P[_]: Pitcher](
    seq: Seq[C],
    backstop: C
  ): P[C] =
    seq.foldRight[P[C]](
      constantly(backstop)
    ) { (c: C, pitcher: P[C]) => construct[C, P](c, pitcher) }
}

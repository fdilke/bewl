package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.VanillaPitcherOld
import com.fdilke.bewl2.topology.Compact._

import scala.annotation.tailrec
import scala.language.postfixOps

object VanillaPitcherOldPredicateSolver {
  def solveSeq[
    C: Compact
  ](
    predicate: VanillaPitcherOld[C] => Boolean
  ): Option[Seq[C]] =
    new VanillaPitcherOldPredicateSolver(
      predicate
    ).tryMap {
        DraftPitcherOld.empty
      }
      .map {
        _.seq
      }

  def solvePitcher[
    C: Compact
  ](
    predicate: VanillaPitcherOld[C] => Boolean
  ): Option[VanillaPitcherOld[C]] =
    Compact[C].optional.flatMap { c =>
      solveSeq(predicate).map(seq => GoPlatinumPitcherOld(seq, c))
    }
}

class VanillaPitcherOldPredicateSolver[
  C: Compact
](
  predicate: VanillaPitcherOld[C] => Boolean
) {
  @tailrec final private def tryMap(
    draft: DraftPitcherOld[C]
  ): Option[DraftPitcherOld[C]] =
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
      case StumpedOldException =>
        Right(())
    }) match {
      case Left(result) => result
      case Right(_) =>
        determine[C] { c =>
          tryMapNonTailRec(
            draft.plus(c)
          ) isDefined
        } match { // exercise for the student, why can't this be a flatmap?
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            tryMap(draft.plus(c))
          case None => None
        }
    }

  private def tryMapNonTailRec(
    draft: DraftPitcherOld[C]
  ): Option[DraftPitcherOld[C]] =
    tryMap(draft)
}

case object StumpedOldException extends Exception

object DraftPitcherOld {
  def empty[C: Compact]: DraftPitcherOld[C] =
    new DraftPitcherOld[C](Seq.empty)
}

class DraftPitcherOld[C](
  val seq: Seq[C]
) extends AnyVal {
  def asPitcher: VanillaPitcherOld[C] =
    new VanillaPitcherOld[C] {
      override lazy val head: C =
        if (seq.isEmpty)
          throw StumpedOldException
        else
          seq.head

      override def tail: VanillaPitcherOld[C] =
        if (seq.isEmpty)
          throw StumpedOldException
        else
          new DraftPitcherOld[C](seq.tail).asPitcher
    }

  def plus(c: C): DraftPitcherOld[C] =
    new DraftPitcherOld[C](seq :+ c)
}

object GoPlatinumPitcherOld {
  def apply[C](
    seq: Seq[C],
    backstop: C
  ): VanillaPitcherOld[C] =
    seq.foldRight[VanillaPitcherOld[C]](
      VanillaPitcherOld.constantly(backstop)
    )((c: C, pitcher: VanillaPitcherOld[C]) => VanillaPitcherOld[C](c, pitcher))
}

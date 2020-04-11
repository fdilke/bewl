package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.VanillaPitcher
import com.fdilke.bewl2.topology.Compact._

import scala.annotation.tailrec
import scala.language.postfixOps

object VanillaPitcherPredicateSolver {
  def solveSeq[
    C: Compact
  ](
    predicate: VanillaPitcher[C] => Boolean
  ): Option[Seq[C]] =
    new VanillaPitcherPredicateSolver(
      predicate
    ) tryMap {
      DraftPitcher.empty
    } map {
      _.seq
    }

  def solvePitcher[
    C: Compact
  ](
    predicate: VanillaPitcher[C] => Boolean
  ): Option[VanillaPitcher[C]] =
    Compact[C].optional flatMap { c =>
      solveSeq(predicate) map { seq => GoPlatinumPitcher(seq, c) }
    }
}

class VanillaPitcherPredicateSolver[
  C: Compact
](
  predicate: VanillaPitcher[C] => Boolean
) {
  @tailrec private final def tryMap(
    draft: DraftPitcher[C]
  ): Option[DraftPitcher[C]] =
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
      case StumpedException =>
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
    draft: DraftPitcher[C]
  ): Option[DraftPitcher[C]] =
    tryMap(draft)
}

case object StumpedException extends Exception

object DraftPitcher {
  def empty[C: Compact]: DraftPitcher[C] =
    new DraftPitcher[C](Seq.empty)
}

class DraftPitcher[C](
  val seq: Seq[C]
) extends AnyVal {
  def asPitcher: VanillaPitcher[C] =
    new VanillaPitcher[C] {
      override lazy val head: C =
        if (seq.isEmpty)
          throw StumpedException
        else
          seq.head

      override def tail: VanillaPitcher[C] =
        if (seq.isEmpty)
          throw StumpedException
        else
          new DraftPitcher[C](seq.tail).asPitcher
    }

  def plus(c: C): DraftPitcher[C] =
    new DraftPitcher[C](seq :+ c)
}

object GoPlatinumPitcher {
  def apply[C](
    seq: Seq[C],
    backstop: C
  ): VanillaPitcher[C] =
    seq.foldRight[VanillaPitcher[C]](
      VanillaPitcher.constantly(backstop)
    ) { (c: C, pitcher: VanillaPitcher[C]) => VanillaPitcher[C](c, pitcher) }
}

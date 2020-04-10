package com.fdilke.bewl2.topology

import com.fdilke.bewl2.cantorians.VanillaPitcher
import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.topology.Hausdorff.Key

import scala.annotation.tailrec
import scala.language.postfixOps

object VanillaPitcherPredicateSolver {
  def solveMap[
    C: Compact
  ](
    predicate: VanillaPitcher[C] => Boolean
  ): Option[DraftPitcher[C]] =
    new VanillaPitcherPredicateSolver(
      predicate
    ) tryMap {
      DraftPitcher.empty
    }

  def solveFunction[
    C: Compact
  ](
    predicate: VanillaPitcher[C] => Boolean
  ): Option[VanillaPitcher[C]] =
    solveMap(predicate) map {
      functionFromMap(_)
    }

  @inline def functionFromMap[
    C: Compact
  ](
    draft: DraftPitcher[C]
  ): VanillaPitcher[C] =
    draft.asPitcher
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
      case StumpedAtException(n) =>
        Right(n)
    }) match {
      case Left(result) => result
      case Right(n) =>
        determine[C] { c =>
          tryMapNonTailRec(
            draft.wIth(n, c)
          ) isDefined
        } match { // exercise for the student, why can't this be a flatmap?
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            tryMap(draft.wIth(n, c))
          case None => None
        }
    }

  private def tryMapNonTailRec(
    draft: DraftPitcher[C]
  ): Option[DraftPitcher[C]] =
    tryMap(draft)

  case class StumpedAtException(
    n: Int
  ) extends Exception
}

object DraftPitcher {
  def empty[C: Compact]: DraftPitcher[C] =
    ???
}

class DraftPitcher[C: Compact] {
  def asPitcher: VanillaPitcher[C] =
    ???

  def wIth(n: Int, c: C): DraftPitcher[C] =
    ???
}

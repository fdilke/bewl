package com.fdilke.bewl2.topology

import java.util.concurrent.atomic.AtomicReference

import Compact._
import Hausdorff._

import scala.annotation.tailrec

sealed trait TryMapResult
object GivenUp extends TryMapResult

sealed trait LearnerState
object Virgin extends LearnerState

class HausdorffToCompactPredicateSolver[
  H: Hausdorff,
  C: Compact
](
  predicate: (H => C) => Boolean
) {
  case class ThatWorks(
    map: Map[H, C]
  ) extends TryMapResult

  case class KeepTrying(
    map: Map[H, C]
  ) extends TryMapResult

  @tailrec final def tryMap(
    map: Map[H, C]
  ): TryMapResult = {
    val learner: Learner =
      new Learner(map)
    val holder = new AtomicReference[H]
    val xx: Either[TryMapResult, H] =
      try {
        Left(
          if (predicate(learner(_)))
            ThatWorks(learner.updatedMap)
          else learner.state match {
            case Virgin => GivenUp
          }
        )
      } catch { case StumpedAtException(h) =>
        Right(h)
      }
    xx match {
      case Left(result) => result
      case Right(h) =>
        find[C] { c =>
          tryMapNonTailRec(map + (h -> c)) != GivenUp
        } map {
          _()
        } match {
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            tryMap(map + (h -> c))
          case None => GivenUp
        }
    }
  }

  def tryMapNonTailRec(
    map: Map[H, C]
  ): TryMapResult =
    tryMap(map)

  class Learner(map: Map[H, C]) {
    var updatedMap: Map[H, C] =
      map
    var state: LearnerState =
      Virgin

    def apply(h: H): C =
      map.getOrElse(
        h,
        throw StumpedAtException(h)
      )
  }

  case class StumpedAtException(
    h: H
  ) extends Exception
}




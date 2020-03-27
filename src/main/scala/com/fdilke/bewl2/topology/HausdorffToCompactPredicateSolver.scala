package com.fdilke.bewl2.topology

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

//  case class StumpedOn(
//    h: H
//  ) extends LearnerState

  // TODO: make this tail recursive
  def tryMap(
    map: Map[H, C]
  ): TryMapResult = {
    val learner: Learner =
      new Learner(map)
    try {
      if (predicate(learner(_)))
        ThatWorks(learner.updatedMap)
      else learner.state match {
        case Virgin => GivenUp
      }
    } catch { case StumpedAtException(h) =>
        find[C] { c =>
          tryMap(map + (h -> c)) != GivenUp
        } map {
          _()
        } match {
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            tryMap(map + (h -> c))
          case None => GivenUp
        }
// TODO: is there  KeepTrying(learner.updatedMap) ?
      }
  }

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

  case class StumpedAtException(h: H) extends Exception
}




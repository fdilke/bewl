package com.fdilke.bewl2.topology

import Compact._

import scala.annotation.tailrec
import scala.language.postfixOps

object HausdorffToCompactPredicateSolver {
  def solveMap[
    H: Hausdorff,
    C: Compact
  ](
     predicate: (H => C) => Boolean
   ): Option[Map[H, C]] =
    new HausdorffToCompactPredicateSolver(
      predicate
    ) tryMap
      Map.empty
}

class HausdorffToCompactPredicateSolver[
  H: Hausdorff,
  C: Compact
](
  predicate: (H => C) => Boolean
) {
  @tailrec private final def tryMap(
    map: Map[H, C]
  ): Option[Map[H, C]] = {
    (try {
      Left(
        if (predicate(h =>
          map.getOrElse(
            h,
            throw StumpedAtException(h)
          )
        ))
          Some(map)
        else
          None
      )
    } catch { case StumpedAtException(h) =>
      Right(h)
    }) match {
      case Left(result) => result
      case Right(h) =>
        find[C] { c =>
          tryMapNonTailRec(
            map + (h -> c)
          ) isDefined
        } map {
          _()
        } match { // exercise for the student, why can't this be a flatmap?
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            tryMap(map + (h -> c))
          case None => None
        }
    }
  }

  private def tryMapNonTailRec(
    map: Map[H, C]
  ): Option[Map[H, C]] =
    tryMap(map)

  // May eventually have to add a sequence number to this, to identify the scope
  case class StumpedAtException(
    h: H
  ) extends Exception
}




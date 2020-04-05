package com.fdilke.bewl2.topology

import Compact._
import Hausdorff.Key

import scala.annotation.tailrec
import scala.language.postfixOps

object HausdorffToCompactPredicateSolver {
  def solveMap[
    H: Hausdorff,
    C: Compact
  ](
     predicate: (H => C) => Boolean
  ): Option[Map[Key[H], C]] =
    new HausdorffToCompactPredicateSolver(
      predicate
    ) tryMap {
      Map.empty
    }

  def solveFunction[
    H: Hausdorff,
    C: Compact
  ](
     predicate: (H => C) => Boolean
  ): Option[H => C] =
     solveMap(predicate) map {
       functionFromMap(_)
     }

  @inline def functionFromMap[
    H: Hausdorff,
    C: Compact
  ] (
    map: Map[Key[H], C]
  ): H => C =
    h => map.getOrElse(
          new Key(h),
          optional[C].get
        )
}

class HausdorffToCompactPredicateSolver[
  H: Hausdorff,
  C: Compact
](
  predicate: (H => C) => Boolean
) {
  @tailrec private final def tryMap(
    map: Map[Key[H], C]
  ): Option[Map[Key[H], C]] =
    (try {
      Left(
        if (predicate(h =>
          map.getOrElse(
            new Key(h),
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
        determine[C] { c =>
          tryMapNonTailRec(
            map + (new Key(h) -> c)
          ) isDefined
        } match { // exercise for the student, why can't this be a flatmap?
          case Some(c) => // TODO: enhance find so we don't do this calculation twice
            tryMap(map + (new Key(h) -> c))
          case None => None
        }
    }

  private def tryMapNonTailRec(
    map: Map[Key[H], C]
  ): Option[Map[Key[H], C]] =
    tryMap(map)

  case class StumpedAtException(
    h: H
  ) extends Exception
}




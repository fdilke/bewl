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
    ).tryMap {
      Map.empty
    }

  def solveFunction[
    H: Hausdorff,
    C: Compact
  ](
    predicate: (H => C) => Boolean
  ): Option[H => C] =
    solveMap(predicate).map {
      functionFromMap(_)
    }

  @inline def functionFromMap[
    H: Hausdorff,
    C: Compact
  ](
    map: Map[Key[H], C]
  ): H => C =
    h =>
      map.getOrElse(
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
  def tryMap(
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
    } catch {
      case StumpedAtException(h) =>
        Right(h)
    }) match {
      case Left(result) => result
      case Right(h) =>
        determine[C, Option[Map[Key[H], C]]](
          c =>
            tryMap(
              map + (new Key(h) -> c)
            ),
          om => om.isDefined
        ).flatMap {
          _._2
        }
    }

  case class StumpedAtException(
    h: H
  ) extends Exception
}

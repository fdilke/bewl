package com.fdilke.bewl2.topology

import com.fdilke.bewl2.compacta.CantorianADTs.Catcher

class DynamicCatcher[S, T] extends Catcher[
  DynamicCatcher[S, T],
  S,
  DynamicCatcher[S, T]
] {
  override def either: Either[
    DynamicCatcher[S, T],
    S => DynamicCatcher[S, T]
  ] = ???
}

class HausdorffToCompactPredicateSolver[
  H: Hausdorff,
  C: Compact
](
  predicate: (H => C) => Boolean
) {
  def tryMap(
    map: Map[H, C]
  ): TryMapResult = {
    ThatWorks
  }
}

sealed trait TryMapResult

object ThatWorks extends TryMapResult
object GivenUp extends TryMapResult
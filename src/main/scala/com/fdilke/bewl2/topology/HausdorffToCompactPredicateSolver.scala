package com.fdilke.bewl2.topology

import com.fdilke.bewl2.compacta.CantorianADTs.Catcher

sealed trait TryMapResult
object GivenUp extends TryMapResult

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

  def tryMap(
    map: Map[H, C]
  ): TryMapResult = {
    val learner: Learner =
      new Learner(map)
    if (predicate(learner(_)))
      ThatWorks(learner.updatedMap)
    else if (learner.isExhausted)
      GivenUp
    else
      KeepTrying(learner.updatedMap)
  }

  class Learner(map: Map[H, C]) {
    var updatedMap: Map[H, C] =
      map
    var isExhausted: Boolean =
      true

    def apply(h: H): C =
      ???
  }
}




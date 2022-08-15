package com.fdilke.algo

import scala.collection.immutable.HashMap

object Backtrack {
  trait DecisionNode[KEY, VALUE] extends Function[
    Map[KEY, VALUE],
    NextStep[KEY, VALUE]
  ]

  sealed trait NextStep[KEY, VALUE]

  case object MapComplete extends NextStep[_, _]
  case object MapInvalid extends NextStep[_, _]
  case class MapContinue[KEY, VALUE](
    requiredKey: KEY,
    node: DecisionNode[KEY, VALUE]
  ) extends NextStep[KEY, VALUE]

  def solve[KEY, VALUE](
     values: Iterable[VALUE],
     initialNode: DecisionNode[KEY, VALUE]
  ): Iterable[Map[KEY, VALUE]] = {
    def recurse(
      partialMap: Map[KEY, VALUE],
      step: NextStep[KEY, VALUE]
    ): Iterable[Map[KEY, VALUE]] =
      step match {
        case MapInvalid =>
          Iterable.empty
        case MapComplete /* .asInstanceOf[NextStep[KEY, VALUE]] */ =>
          Iterable(partialMap)
        case MapContinue(key, node) =>
          for {
            value <- values
            newMap = partialMap + (key -> value)
            solution <- recurse(newMap, node(newMap))
          } yield
            solution
      }

    val initialMap: Map[KEY, VALUE] = new HashMap[KEY, VALUE]()
    recurse(initialMap, initialNode(initialMap))
  }
}

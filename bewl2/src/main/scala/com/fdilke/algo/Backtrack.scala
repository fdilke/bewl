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
    ): Iterator[Map[KEY, VALUE]] =
      step match {
        case MapInvalid =>
          Iterator.empty
        case MapComplete /* .asInstanceOf[NextStep[KEY, VALUE]] */ =>
          Iterator(partialMap)
        case MapContinue(key, node) =>
          for {
            value <- values.iterator
            newMap = partialMap + (key -> value)
            solution <- recurse(newMap, node(newMap))
          } yield
            solution
      }

    val initialMap: Map[KEY, VALUE] = new HashMap[KEY, VALUE]()
    new Iterable[Map[KEY, VALUE]] {
      override def iterator: Iterator[Map[KEY, VALUE]] =
        recurse(initialMap, initialNode(initialMap))
    }
  }
}

package com.fdilke.bewl2.sets

object SetsUtilities {
  def allMaps[A, B](
     source: Set[A],
     target: Set[B]
  ): Iterable[Map[A, B]] =
    if (source.isEmpty)
      Iterable(Map.empty)
    else
      for {
        partialMap <- allMaps(source.tail, target)
        choice <- target
      } yield {
        partialMap + (source.head -> choice)
      }
}

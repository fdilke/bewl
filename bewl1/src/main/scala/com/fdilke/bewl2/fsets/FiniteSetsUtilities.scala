package com.fdilke.bewl2.fsets

object FiniteSetsUtilities {

  def allMaps[A, B](
    source: Iterable[A],
    target: Iterable[B]
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

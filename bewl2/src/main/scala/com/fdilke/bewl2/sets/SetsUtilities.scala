package com.fdilke.bewl2.sets

object SetsUtilities:
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

  trait VarArgFunc[-A, +B]:
    def apply(is: A*): B

  def allNaryOps(
    arity: Int,
    order: Int
  ): Iterable[VarArgFunc[Int, Int]] = {
    val toOrder: Seq[Int] = (0 until order)
    val toArity: Seq[Int] = (0 until arity)
    val source: Iterable[Map[Int, Int]] =
      allMaps(toArity, toOrder)
    allMaps(source, toOrder) map { (m: Map[Int, Int] => Int) =>
      (a: Seq[Int]) =>
        val x: Map[Int, Int] = Map(
          toArity map { i => i -> a(i) } :_*
        )
        m(x)
    }
  }

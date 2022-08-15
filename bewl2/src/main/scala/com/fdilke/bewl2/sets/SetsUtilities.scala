package com.fdilke.bewl2.sets

import scala.language.postfixOps

object SetsUtilities:
  def allMaps[A, B](
     source: Iterable[A],
     target: Iterable[B]
  ): Iterable[Map[A, B]] =
    if (source.isEmpty)
      Iterable(Map.empty)
    else
      (for {
        partialMap <- allMaps(source.tail, target)
        choice <- target
      } yield {
        partialMap + (source.head -> choice)
      }).view

  trait VarArgFunc[-A, +B]:
    def apply(is: A*): B

  private def seqToMap[T](
    a: Seq[T]
  ): Map[Int, T] =
    Map(
      a.indices map { i =>
        i -> a(i)
      } :_*
    )

  def allNaryOps(
    arity: Int,
    order: Int
  ): Iterable[VarArgFunc[Int, Int]] = {
    val toOrder: Seq[Int] = (0 until order)
    val toArity: Seq[Int] = (0 until arity)

    allMaps(
      allMaps(toArity, toOrder),
      toOrder
    ) map { (m: Map[Int, Int] => Int) =>
      (a: Seq[Int]) =>
        m(seqToMap(a))
    }
  }

package com.fdilke.bewl2.helper

object Memoize {

  def vanilla[A, B](
    fn: A => B
  ): A => B = {
    val resultMap =
      scala.collection.mutable.Map[A, B]()

    a => resultMap.getOrElseUpdate(
          a,
          fn(a)
        )
  }

  def apply[
    IN[_, _],
    OUT[_, _]
  ](
    fn: [X, Y] => IN[X, Y] => OUT[X, Y]
  ): [X, Y] => IN[X, Y] => OUT[X, Y] =
    [X, Y] =>
      (input: IN[X, Y]) =>
        fn[X, Y](
          input
        )
}

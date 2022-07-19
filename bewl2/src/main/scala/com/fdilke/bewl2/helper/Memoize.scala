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
  ): [X, Y] => IN[X, Y] => OUT[X, Y] = {
    def erasedFn(
      in: IN[Nothing, Nothing]
    ): OUT[Nothing, Nothing] =
      fn[Nothing, Nothing](
        in
      )
    val memoizedErased =
      vanilla[
        IN[Nothing, Nothing],
        OUT[Nothing, Nothing]
      ](erasedFn)

    [X, Y] =>
      (input: IN[X, Y]) =>
        memoizedErased(
          input.asInstanceOf[IN[Nothing, Nothing]]
        ).asInstanceOf[OUT[X, Y]]
  }
}

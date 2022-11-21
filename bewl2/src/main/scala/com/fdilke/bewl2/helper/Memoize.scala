package com.fdilke.bewl2.helper

import scala.collection.mutable

object Memoize {

  def apply[A, B](
    fn: A => B
  ): A => B =
    val resultMap: mutable.Map[A, B] =
      mutable.Map[A, B]()

    a => resultMap.getOrElseUpdate(a, fn(a))

  def type1[
    IN[_],
    OUT[_]
  ](
    fn: [X] => IN[X] => OUT[X]
  ): [X] => IN[X] => OUT[X] = {
    def erasedFn(
      in: IN[Nothing]
    ): OUT[Nothing] =
      fn[Nothing](
        in
      )
    val memoizedErased =
      Memoize[
        IN[Nothing],
        OUT[Nothing]
      ](erasedFn)

    [X] =>
      (input: IN[X]) =>
        memoizedErased(
          input.asInstanceOf[IN[Nothing]]
        ).asInstanceOf[OUT[X]]
  }

  def type2[
    IN[_, _],
    OUT[_, _]
  ](
    fn: [X, Y] => IN[X, Y] => OUT[X, Y]
  ): [X, Y] => IN[X, Y] => OUT[X, Y] = {
    def erasedFn(
      in: IN[Nothing, Nothing]
    ): OUT[Nothing, Nothing] =
      fn[Nothing, Nothing](in)
    val memoizedErased: IN[Nothing, Nothing] => OUT[Nothing, Nothing] =
      Memoize[
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

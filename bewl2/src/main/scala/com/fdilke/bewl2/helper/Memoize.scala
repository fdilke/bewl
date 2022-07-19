package com.fdilke.bewl2.helper

object Memoize {

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

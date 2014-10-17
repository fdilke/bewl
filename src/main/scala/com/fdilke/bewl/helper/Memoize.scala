package com.fdilke.bewl.helper

object Memoize {
  class MemoizedFunction[INPUT[T <: BASE], OUTPUT[T <: BASE], BASE](function: INPUT[_ <: BASE] => OUTPUT[_ <: BASE]) {

    private val resultMap = scala.collection.mutable.Map[INPUT[_], OUTPUT[_]]()

    def apply[T <: BASE](input: INPUT[T]): OUTPUT[T] =
      resultMap.getOrElseUpdate(input, function(input)).asInstanceOf[OUTPUT[T]]
  }

  def apply[INPUT[T], OUTPUT[T]](function: INPUT[Nothing] => OUTPUT[Nothing]) =
    new MemoizedFunction[INPUT, OUTPUT, Any](
      function.asInstanceOf[INPUT[_] => OUTPUT[_]]
    )

  def withLowerBound[INPUT[T <: BASE], OUTPUT[T <: BASE], BASE](function: INPUT[BASE] => OUTPUT[BASE]) =
    new MemoizedFunction[INPUT, OUTPUT, BASE](
      function.asInstanceOf[INPUT[_ <: BASE] => OUTPUT[_ <: BASE]]
    )
}



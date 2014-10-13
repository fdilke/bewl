package com.fdilke.bewl.helper

object Memoize {
  class MemoizedFunction[INPUT[T], OUTPUT[T]](function: INPUT[_] => OUTPUT[_]) {

    private val resultMap = scala.collection.mutable.Map[INPUT[_], OUTPUT[_]]()

    def apply[T](input: INPUT[T]): OUTPUT[T] =
      resultMap.getOrElseUpdate(input, function(input)).asInstanceOf[OUTPUT[T]]
  }

  def apply[INPUT[T], OUTPUT[T], U](function: INPUT[U] => OUTPUT[U]) =
    new MemoizedFunction[INPUT, OUTPUT](
      function.asInstanceOf[INPUT[_] => OUTPUT[_]]
    )
}



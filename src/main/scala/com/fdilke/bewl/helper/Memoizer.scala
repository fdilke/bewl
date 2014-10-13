package com.fdilke.bewl.helper

object Memoizer {
  private val theMemoizer = new Memoizer[Seq, Seq]

  def apply[INPUT[T], OUTPUT[T]]() : Memoizer[INPUT, OUTPUT] =
    theMemoizer.asInstanceOf[Memoizer[INPUT, OUTPUT]]
}

class MemoizedFunction[INPUT[T], OUTPUT[T]](function: INPUT[_] => OUTPUT[_]) {

  private val resultMap = scala.collection.mutable.Map[INPUT[_], OUTPUT[_]]()

  def apply[T](input: INPUT[T]): OUTPUT[T] =
    resultMap.getOrElseUpdate(input, function(input)).asInstanceOf[OUTPUT[T]]
}

class Memoizer[INPUT[T], OUTPUT[T]] {
  def apply[T](function: INPUT[T] => OUTPUT[T]) = new MemoizedFunction[INPUT, OUTPUT](
    function.asInstanceOf[INPUT[_] => OUTPUT[_]]
  )
}

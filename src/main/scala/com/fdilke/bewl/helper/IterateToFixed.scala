package com.fdilke.bewl.helper

import scala.annotation.tailrec

object IterateToFixed {

  def apply[T](
    initial: T
  )(
    fn: T => T
  ) = {
    @tailrec def iterateOn(
      value: T
    ): T = {
      val next = fn(value)
      if (next == value)
        value
      else
        iterateOn(next)
    }
    iterateOn(initial)
  }
}

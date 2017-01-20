package com.fdilke.bewl.helper

object Timed {
  def apply[T](
    message: String
  )(
    fn: => T
  ) = {
    val start = System.currentTimeMillis()
    val result = fn
    val end = System.currentTimeMillis()
    val seconds = (end - start)/1000.0
    println(message + f" took: $seconds%1.2f sec")
    result
  }
}

package com.fdilke.bewl.helper

object Timed {
  def apply[T](
    message: String
  )(
    fn: => T
  ): T = {
    print(message + " ...")
    val start = System.currentTimeMillis()
    val result = fn
    val end = System.currentTimeMillis()
    val seconds = (end - start) / 1000.0
    println(f" took: $seconds%1.2f sec")
    result
  }
}

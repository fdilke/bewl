package com.fdilke.bewl.helper

object DumpStack extends App {
  def apply(message: String) = {
    println(message + " tracing...")
    val traces: Array[StackTraceElement] =
      (new RuntimeException).getStackTrace
    for { trace <- traces } {
      println("\t" + trace)
    }
    println(message + " ...done")
  }
}

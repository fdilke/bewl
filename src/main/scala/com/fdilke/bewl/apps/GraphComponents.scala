package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSetsUtilities._

object GraphComponents extends App {
  val vertices = dot(1, 2, 3, 4, 5)
  val edges = dot(
    1 -> 2,
    3 -> 4,
    4 -> 5
//    4 -> 6
  )

  val f1 = edges(vertices) { _._1 }
  val f2 = edges(vertices) { _._2 }

  val coequalizer =
    f1 =? f2

  private val components =
    coequalizer.arrow.target

  val numComponents = components.globals.size
  println(s"$numComponents components")

  for { c <- elementsOf(components) } {
    print("component: { ")
    for { v <- elementsOf(vertices) }
      if (c(v))
        print(s"$v ")
    println("}")
  }
}

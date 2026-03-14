package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.Timed

object GraphComponents extends App {

  val vertices = dot(1, 2, 3, 4, 5, 6)
  val edges = dot(
    1 -> 2,
    3 -> 4,
    4 -> 5,
    4 -> 6
  )

  val f1 = edges(vertices) {
    _._1
  }
  val f2 = edges(vertices) {
    _._2
  }

  val coequalizer =
    Timed("calculating the components") {
      f1 =?! f2
    }

  val components =
    coequalizer.arrow.target

  val numComponents = components.size
  println(s"$numComponents components")

  import vertices.power.{evaluate => $}
  for { c <- elementsOf(components) } {
    print("component: { ")
    for { v <- elementsOf(vertices) } if ($(c, v))
      print(s"$v ")
    println("}")
  }
}

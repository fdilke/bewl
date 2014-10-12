package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot

// Originally for finding 'difference-preserving binary operators'.
// It doesn't find any up to n=3.
// Also serves as a cue to fix allMaps() which seems to break on 4 ^ 16, never enumerating anything.

object DPFunctions extends App {

  def countDP(n: Int) = {
    val sizeN = DiagrammaticFiniteSetsDot(0 until n)
    val pairs = sizeN x sizeN
    var numExamined = 0

    def isDifferencePreserving(f: ((Int, Int)) => Int) = (
      for (x <- sizeN;
           y <- sizeN if x != y;
           p <- sizeN;
           q <- sizeN if p != q)
      yield f((x, p)) != f((y, q))
      ).forall(identity)

    def showIfDifferencePreserving(f: ((Int, Int)) => Int) = {
      numExamined = numExamined + 1
      if (numExamined % 100 == 0) {
        println(s"[$numExamined]")
      }
      if (isDifferencePreserving(f)) {
        for (x <- sizeN;
             y <- sizeN) {
          print(s"$x$y=${f((x,y))} ")
        }
        println
        true
      } else false
    }

    (sizeN ^ pairs).count(showIfDifferencePreserving)
  }

  println("DP:")
  for (n <- 0 to 4) {
    println(s"$n -> ${countDP(n)}")
  }
}

package com.fdilke.bewl.helper

import scala.language.postfixOps

// A 0-based, functional tribute to the original Numerical Recipes
// algorithm: http://www.aip.de/groups/soe/local/numres/bookcpdf/c8-6.pdf

object BuildEquivalence {

  def apply(
    size: Int,
    relators: Traversable[(Int, Int)]
  ): Seq[Int] = {
    val range =
      0 until size

    val array: Array[Int] =
      range toArray

    val trackUp: Int => Int =
      IterateToFixed(_)(array)

    def equate(
      i: Int,
      j: Int
    ) {
      if (i != j)
        array(i) = j
    }

    for {
      (j, k) <- relators
    }
      equate(
        trackUp(j),
        trackUp(k)
      )

    for { i <- range }
      array(i) =
          trackUp(array(i))

    array
  }
}
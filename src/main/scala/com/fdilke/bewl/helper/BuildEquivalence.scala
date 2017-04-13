package com.fdilke.bewl.helper

import scala.annotation.tailrec

object BuildEquivalence {
  def apply(
    size: Int,
    relators: Traversable[(Int, Int)]
  ): Seq[Int] = {
    val array = (0 until size).toArray

    def trackUp(i: Int) =
      IterateToFixed(i)(array)

    for {
      (j, k) <- relators
      jj = trackUp(j)
      kk = trackUp(k)
    } 
      if (jj != kk)
        array(jj) = kk

// exercise for the student: why doesn't this work?
//        array map trackUp
// instead we have to do:
        
    for { i <- 1 until size } {
      while (array(i) != array(array(i)))
          array(i) = array(array(i))
    }
	  
    array
  }
}
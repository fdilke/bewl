package com.fdilke.bewl.helper

import scala.annotation.tailrec

object BuildEquivalence {
  def apply(
    size: Int,
    relators: Traversable[(Int, Int)]
  ): Seq[Int] = {
    val array = (0 until size).toArray

    @tailrec def trackUp(i: Int): Int = {
      val ii = array(i)
      if (i == ii)
        i
      else
        trackUp(ii)
    }
    
    for {
      (j, k) <- relators
      jj = trackUp(j)
      kk = trackUp(k)
    } 
      if (jj != kk)
        array(jj) = kk
    
    for {
      j <- 0 until size
    }
      while (array(j) != array(array(j))) 
        array(j) = array(array(j))
      
    array
  }
}
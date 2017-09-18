package com.fdilke.bewl.helper

object BuildEquivalence {
  def apply(
    size: Int,
    relators: Traversable[(Int, Int)]
  ): Seq[Int] = {
    val array: Array[Int] =
      (0 until size).toArray

    var j, k = 0

    //    for (k<-0 until n) nf(k)=k /* Initialize each element its own class. */
    for ((jj, kk) <- relators) {
      /* For each piece of input information... */
      j = jj
      while (array(j) != j) j = array(j) /* Track first element up to its ancestor. */
      k = kk

      while (array(k) != k) k = array(k) /* Track second element up to its ancestor. */
      if (j != k) array(j) = k /* If they are not already related, make them so */
    }

    for (j <- 0 until size) /* Final sweep up to highest ancestors. */
      while (array(j) != array(array(j))) array(j) = array(array(j))

    array
  }
/*
  def apply(
    size: Int,
    relators: Traversable[(Int, Int)]
  ): Seq[Int] = {
    val array: Array[Int] =
      (0 until size).toArray

//    def trackUp(i: Int) =
//      IterateToFixed(i)(array)
    def trackUp(i: Int) = {
      var ii = i;
      while (ii != array(ii))
        ii = array(ii)
      ii
    }

    for {
      (j, k) <- relators
      _ = println(s"ZZZ relator: $j -> $k")
      jj = trackUp(j)
      kk = trackUp(k)
    } 
      if (jj != kk)
        array(jj) = kk

// exercise for the student: why doesn't this work?
//        array map trackUp
// instead we have to do:

    println("ZZZ Tracking up... array = " + (array.toSeq.mkString(":")))
    for { i <- 0 until size } {
      while (array(i) != array(array(i)))
          array(i) = array(array(i))
    }
    println("ZZZ Tracking up... done")

    array
  }
*/
}
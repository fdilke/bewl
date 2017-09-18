package com.fdilke.bewl

import scala.language.postfixOps

object AwkwardArrays extends App {
  val array =
    Array(3, 3, 3, 2)

  def square(array: Array[Int]): Array[Int] =
    array.indices map {
      i => array(array(i))
    } toArray

  def eclass(nf: Array[Int], n: Int, lista: Array[Int], listb: Array[Int], m: Int) {
    var j, k = 0

    for (k <- 0 until n) nf(k) = k /* Initialize each element its own class. */
    for (l <- 0 until m) {
      /* For each piece of input information... */
      j = lista(l)
      while (nf(j) != j) j = nf(j) /* Track first element up to its ancestor. */
      k = listb(l)

      while (nf(k) != k) k = nf(k) /* Track second element up to its ancestor. */
      if (j != k) nf(j) = k /* If they are not already related, make them so */
    }

    for (j <- 0 until n) /* Final sweep up to highest ancestors. */
      while (nf(j) != nf(nf(j))) nf(j) = nf(nf(j))
  }

  printf("Hello NR");
  val nf = Array(0, 1, 2, 3)
  val n = 4
  val lista = Array(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)
  val listb = Array(0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3)
  val m = 16;
  eclass(nf, n, lista, listb, m)
  printf("nf is now:")
  for (i <- 0 until n)
    printf("" + nf(i) + ":")

  def buildEq(
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

  println("Ans:")
  val ans =
    buildEq(
      4,
      Traversable(
        0 -> 0,
        0 -> 1,
        0 -> 2,
        0 -> 3,
        1 -> 0,
        1 -> 1,
        1 -> 2,
        1 -> 3,
        2 -> 0,
        2 -> 1,
        2 -> 2,
        2 -> 3,
        3 -> 0,
        3 -> 1,
        3 -> 2,
        3 -> 3
      )
    )
  println("Ans: " + ans)
}

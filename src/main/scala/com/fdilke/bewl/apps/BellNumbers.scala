package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕

object BellNumbers extends App {

  def bell(n: Int): Int = {
    val set: DOT[Int] = dot(0 until n :_*)
    val set2 = set.squared

    val eqRelns = set2.power.whereAll(set) {
      (ssp, s) =>
        ssp(set.squared.pair(s, s))
    }.whereAll(set, set) {
      (ssp, x, y) =>
        ssp(set.squared.pair(x, y)) →
        ssp(set.squared.pair(y, x))
    }.whereAll(set, set, set) {
      (ssp, x, y, z) =>
        ssp(set.squared.pair(x, y)) ∧
        ssp(set.squared.pair(y, z)) →
        ssp(set.squared.pair(x, z))
    }
    elementsOf(eqRelns).size
  }

  for {
    n <- 0 to 10
  } {
    val start = System.currentTimeMillis
    val bellNo = bell(n)
    val time = System.currentTimeMillis - start
    println(s"$n -> ${bellNo}\t\t(${time}ms)")
  }
}
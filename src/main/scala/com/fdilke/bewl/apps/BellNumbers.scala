package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕

object BellNumbers extends App {

  def bell(n: Int): Int = {
    val set = dot(0 to n :_*)
    val set2 = set.squared

    val eqRelns = set2.power where { ssp =>
      set2.power.forAll(set) {
        (ssp, s) =>
          ssp(set.squared.pair(s, s))
      }(ssp)
    } where { ssp =>
      set2.power.forAll(set, set) {
        (ssp, x, y) =>
          ssp(set.squared.pair(x, y)) →
          ssp(set.squared.pair(y, x))
      }(ssp)
    } where { ssp =>
      set2.power.forAll(set, set, set) {
        (ssp, x, y, z) =>
          ssp(set.squared.pair(x, y)) ∧
          ssp(set.squared.pair(y, z)) →
          ssp(set.squared.pair(x, z))
      }(ssp)
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

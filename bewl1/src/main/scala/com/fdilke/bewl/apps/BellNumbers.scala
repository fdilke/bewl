package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import scala.language.postfixOps

//  final def isEquivalenceRelation(
//    equiv: (S, S) => TRUTH
//  ) = // TODO: rewrite this using first order logic
//    this.universally { s =>
//      equiv(s, s)
//    } &&
//      squared.universally {
//        case s ⊕ t =>
//          equiv(s, t) → equiv(t, s)
//      } &&
//      (squared x dot).universally {
//        case s ⊕ t ⊕ u  =>
//          equiv(s, t) ∧ equiv(t, u) → equiv(s, u)
//      }

object BellNumbers extends App {

  def slowBell(n: Int): Int = {
    val set: DOT[Int] = dot(0 until n: _*)
    implicit val set2 = set.squared
    implicit val anonImplicit = set2.power

    val eqRelns = set2.power
      .whereAll(set)((ssp, s) => ssp(s ⊕⊕ s))
      .whereAll(set, set) { (ssp, x, y) =>
        ssp(x ⊕⊕ y) →
          ssp(y ⊕⊕ x)
      }
      .whereAll(set, set, set) { (ssp, x, y, z) =>
        ssp(x ⊕⊕ y) ∧
          ssp(y ⊕⊕ z) →
          ssp(x ⊕⊕ z)
      }
    eqRelns size
  }

  def bell(n: Int): Int =
    dot(
      0 until n: _*
    ).congruences size

  for {
    n <- 0 to 10
  } {
    val start = System.currentTimeMillis
    val bellNo = bell(n)
    val time = System.currentTimeMillis - start
    println(s"$n -> ${bellNo}\t\t(${time}ms)")
  }
}

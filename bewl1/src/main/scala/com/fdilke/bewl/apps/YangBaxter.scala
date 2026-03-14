package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._

object YangBaxter extends App {
  val B = dot(1, 2)
  val BxB = B.x(B)
  val BxBxB = BxB.x(B)
  val isSolution =
    (BxB > BxB)(omega) { f =>
      val xx = (BxB > ((BxB.π1.o(BxBxB.π0)).x(BxBxB.π1)))(f)

      /*
      val hh: (((Int x Int) x Int) → (Int x Int)) = (BxB > BxBxB.π0)(f)
       */
//      val _f_1 = (BxB > BxBxB.π0)(f) x BxBxB.π1

//      val _1_f = (
//        (BxB.π0 o BxBxB.π0) x (BxB.π0 o xx)
//       ) x (
//          BxB.π1 o xx
//       )
      ???
    }
}

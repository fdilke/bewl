package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.topos.Topos

object FiniteSets {
  implicit object FiniteSetsTopos extends Topos[Set] {
    override def sanityTest[S: Set]: Unit =
      println(s"Sanity testing ${ implicitly[Set[S]] }")

    override def sanityTest[S: Set, T: Set](arrow: S => T): Unit =
      println(s"Sanity testing a function of sets... how?")
  }
}


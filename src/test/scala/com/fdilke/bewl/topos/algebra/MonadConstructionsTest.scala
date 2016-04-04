package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.{Ignore, FunSpec}
import org.scalatest.Matchers._

class MonadConstructionsTest extends FunSpec {
  private val two = dot('x, 'y)

  import FiniteSets._

  describe("The double-exponential monad") {
    ignore("can be constructed for sets") {
      val monadJoin = omega.doubleExpMonad

      monadJoin(O).globals should have size 2
      monadJoin(I).globals should have size 4
      monadJoin(omega).globals should have size 16

      println("Testing 1")
      monadJoin.sanityTestAt(dot(1))
      println("Testing 1 2")
      monadJoin.sanityTestAt(dot(1,2))
      println("Testing 1 2 3")
      monadJoin.sanityTestAt(dot(1,2,3)) // a bridge too far?
      println("Testing 1 2 3 ... done")
    }
  }
}

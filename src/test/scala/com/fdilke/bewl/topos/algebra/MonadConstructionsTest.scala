package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class MonadConstructionsTest extends FunSpec {
  private val two = dot('x, 'y)

  import FiniteSets._

  describe("The double-exponential monad") {
    it("can be constructed for sets") {
      val monadJoin = omega.doubleExpMonad

      monadJoin(O).free.globals should have size 2
      monadJoin(I).free.globals should have size 4
      monadJoin(omega).free.globals should have size 16

      val atTwo = monadJoin(two)
      val eta: Symbol > (Symbol → TRUTH → TRUTH) = atTwo.eta
      for (
        global <- (two > omega).globals ;
        symbol <- Seq('x, 'y)
      ) {
        val f: Symbol → TRUTH = global(())
        eta(symbol)(f) shouldBe f(symbol)
      }

      val symbols = dot('a, 'b)
      val ints = dot(1, 2, 3)
      val f: Symbol > Int = arrow(symbols, ints, 'a -> 2, 'b -> 1)
      val map: ((Symbol → TRUTH) → TRUTH) > ((Int → TRUTH) → TRUTH) = monadJoin.map(f)

      for (
        soo <- (symbols > omega > omega).globals map { _(()) };
        io <- (ints > omega).globals map { _(()) };
        symbol <- Seq('a, 'b)
      )
        map(soo)(io) shouldBe soo((omega > f)(io))

      val mu: (UNIT → TRUTH → TRUTH → TRUTH → TRUTH) > (UNIT → TRUTH → TRUTH) = monadJoin(I).mu

      val io2iooo: (UNIT → TRUTH) > (UNIT → TRUTH → TRUTH → TRUTH) =
        (I > omega > omega > omega).transpose(I > omega) {
          (x, f) => f(x)
        }

      for (
        ioooo <- (I > omega > omega > omega > omega).globals map { _(()) } ;
        io <- (I > omega).globals map { _(()) }
      )
        mu(ioooo)(io) shouldBe ioooo(io2iooo(io))

// does there need to be a 'further internalized contravariant functor' "omega → io" ?

      //      println("Testing 1")
//      monadJoin.sanityTestAt(dot(1))
//      println("Testing 1 2")
//      monadJoin.sanityTestAt(dot(1,2))
//      println("Testing 1 2 3")
//      monadJoin.sanityTestAt(dot(1,2,3)) // a bridge too far?
//      println("Testing 1 2 3 ... done")

// check that M[X] is a monad, via multiplication as structure map. Or is that
// too hard to calculate / a trivial consequence of our existing laws
    }
  }
}

package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FunSpec
import org.scalatest.Matchers._

class MonadConstructionsTest extends FunSpec {
  private val two = dot('x, 'y)

  import FiniteSets._

  val monadJoin = omega.doubleExpMonad

  describe("The double-exponential monad can be constructed for sets, and...") {
    it("values at a dot are cached") {
      (monadJoin(O) eq monadJoin(O)) shouldBe true
    }

    it("free objects have the right size") {
      monadJoin(O).free.globals should have size 2
      monadJoin(I).free.globals should have size 4
      monadJoin(omega).free.globals should have size 16
    }

    it("embedding (eta) works") {
      val eta: Symbol > (Symbol → TRUTH → TRUTH) = monadJoin(two).eta
      for (
        f <- elementsOf(two > omega);
        symbol <- Seq('x, 'y)
      )
        eta(symbol)(f) shouldBe f(symbol)
    }

    it("functoriality (map) works") {
      val symbols = dot('a, 'b)
      val ints = dot(1, 2, 3)
      val f: Symbol > Int = arrow(symbols, ints, 'a -> 2, 'b -> 1)
      val map: (Symbol → TRUTH → TRUTH) > (Int → TRUTH → TRUTH) = monadJoin.map(f)

      // TODO: abstract away 'io' here
      for (
        soo <- elementsOf(symbols > omega > omega);
        io <- elementsOf(ints > omega);
        symbol <- Seq('a, 'b)
      )
        map(soo)(io) shouldBe soo((omega > f) (io))
    }

    it("multiplication (mu) works") {
      val mu: (UNIT → TRUTH → TRUTH → TRUTH → TRUTH) > (UNIT → TRUTH → TRUTH) = monadJoin(I).mu

      val io2iooo: (UNIT → TRUTH) > (UNIT → TRUTH → TRUTH → TRUTH) =
        (I > omega > omega > omega).transpose(I > omega) {
          (x, f) => f(x)
        }

      // TODO: abstract away 'io' here, so we can compare ioooo and (ioooo o io2iooo) directly
      for (
        ioooo <- elementsOf(I > omega > omega > omega > omega) ;
        io <- elementsOf(I > omega)
      )
        mu(ioooo)(io) shouldBe ioooo(io2iooo(io))

      monadJoin(I).sanityTest
      // Can't run sanityTest2, would take too long (2 ^ 2 ^ 2 ^ 2 ^ 2 ^ 2 ^ 1)

// TODO: check that M[X] is a monad, via multiplication as structure map. Or is that...
// TODO: ...too hard to calculate / a trivial consequence of our existing laws?
    }
  }
}

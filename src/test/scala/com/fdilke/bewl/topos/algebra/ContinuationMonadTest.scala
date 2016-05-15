package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.{Matchers, FreeSpec}

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import Matchers._

class ContinuationMonadTest extends FreeSpec {
  private val two = dot('x, 'y)

  import FiniteSets._

  val continuation = omega.continuationMonad

  "For the continuation (double-exponential) monad on sets" - {
    "values at a dot are cached" in {
      (continuation(O) eq continuation(O)) shouldBe true
    }

    "free objects have the right size" in {
      continuation(O).free.globals should have size 2
      continuation(I).free.globals should have size 4
      continuation(omega).free.globals should have size 16
    }

    "embedding (eta) works" in {
      val eta: Symbol > (Symbol → TRUTH → TRUTH) = continuation(two).eta
      for {
        f <- elementsOf(two > omega)
        symbol <- Seq('x, 'y)
      }
        eta(symbol)(f) shouldBe f(symbol)
    }

    "functoriality (map) works" in {
      val symbols = dot('a, 'b)
      val ints = dot(1, 2, 3)
      val f: Symbol > Int = arrow(symbols, ints, 'a -> 2, 'b -> 1)
      val map: (
        Symbol → TRUTH → TRUTH
      ) > (
        Int → TRUTH → TRUTH
      ) = continuation map f

      implicit val forSoo = symbols > omega > omega

      for {
        soo <- elementsOf(symbols > omega > omega)
        soo_ : RichExponential[Symbol → TRUTH, TRUTH] = soo
      }
        map(soo) shouldBe (soo_ o (omega > f))
    }

    "tensorial strength is defined correctly" in {
      val ints = dot(1, 2)
      val symbols = dot('a, 'b)

      val mSymbol = continuation(symbols).free
      val mSymbolElements: List[Symbol → Boolean → Boolean] =
        elementsOf(mSymbol).toList

      mSymbolElements should have size 16
      val arrow = ints(mSymbol) { i =>
        mSymbolElements(i * 3)
      }
      val ⊗ = continuation(ints).tensorialStrength(symbols)
      ⊗ should have (
        'source(ints x mSymbol),
        'target(continuation(ints x symbols).free)
      )
    }

    "multiplication (mu) works" in {
      val mu: (
        UNIT → TRUTH → TRUTH → TRUTH → TRUTH
      ) > (
        UNIT → TRUTH → TRUTH
      ) = continuation(I).mu

      val io2iooo: (UNIT → TRUTH) > (UNIT → TRUTH → TRUTH → TRUTH) =
        (I > omega > omega > omega).transpose(I > omega) {
          (x, f) => f(x)
        }

      implicit val forIoooo = I > omega > omega > omega > omega

      for {
        ioooo <- elementsOf(I > omega > omega > omega > omega)
        ioooo_ : RichExponential[UNIT → TRUTH → TRUTH → TRUTH, TRUTH] = ioooo
      }
        mu(ioooo) shouldBe (ioooo_ o io2iooo)

      continuation(I).sanityTest
      // Can't run sanityTest2, would take too long (2 ^ 2 ^ 2 ^ 2 ^ 2 ^ 2 ^ 1)

// TODO: check that M[X] is a monad, via multiplication as structure map. Or is that...
// TODO: ...too hard to calculate / a trivial consequence of our existing laws?
    }

    "defines a valid home algebra structure map" in {
      continuation.home.structure should have (
        'source(omega > omega > omega),
        'target(omega)
      )

      val id: TRUTH → TRUTH =
        asElement(
          omega.identity
        )

      for (
        oo <- elementsOf(omega > omega > omega)
      )
        continuation.home.structure(
          oo
        ) shouldBe oo(id)

      continuation.home.sanityTest
    }
  }
}

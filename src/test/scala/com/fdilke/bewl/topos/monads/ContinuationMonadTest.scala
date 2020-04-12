package com.fdilke.bewl.topos.monads

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.ContinuousIntegration.notOnCI
import com.fdilke.bewl.helper.⊕._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.{a => _, _}
import com.fdilke.bewl.helper.StandardSymbols._

import scala.language.{implicitConversions, postfixOps, reflectiveCalls}

class ContinuationMonadTest extends AnyFreeSpec {
  private val two = dot(x, y)

  import FiniteSets._

  private val continuation: ContinuationMonad[TRUTH] =
    continuationMonad(omega)

  "For the continuation (double-exponential) monad on sets" - {
    "values at a dot are cached" in {
      (continuation(O) eq continuation(O)) shouldBe true
    }

    "free objects have the right size" in {
      continuation(O).free should have size 2
      continuation(I).free should have size 4
      continuation(omega).free should have size 16
    }

    "embedding (eta) works" in {
      val eta: Symbol > (Symbol → TRUTH → TRUTH) =
        continuation(two).eta

      for {
        f <- elementsOf(two > omega)
        symbol <- Seq(x, y)
      } eta(symbol)(f) shouldBe f(symbol)
    }

    "functoriality (map) works" in {
      val symbols = dot(a, b)
      val ints = dot(1, 2, 3)
      val f: Symbol > Int = arrow(symbols, ints)(
        a -> 2,
        b -> 1
      )
      val map: (
        Symbol → TRUTH → TRUTH
      ) > (
        Int → TRUTH → TRUTH
      ) = continuation.map(f)

      implicit val forSoo: EXPONENTIAL[Symbol → TRUTH, TRUTH] =
        symbols > omega > omega

      for {
        soo <- elementsOf(symbols > omega > omega)
        soo_ : RichExponential[Symbol → TRUTH, TRUTH] = soo
      } map(soo) shouldBe (soo_.o(omega > f))
    }

    "tensorial strength is calculated correctly" in notOnCI {
      val ints = dot(1, 2)
      val symbols = dot(a, b)

      val mSymbols = continuation(symbols).free

      val strength: (
        (
          Int x (Symbol → TRUTH → TRUTH)
        ) > (
          (Int x Symbol) → TRUTH → TRUTH
        )
      ) =
        continuation(
          ints
        ).tensorialStrength(
          symbols
        )

      strength should have(
        source(ints.x(mSymbols)),
        target(
          continuation(
            ints.x(symbols)
          ).free
        )
      )

      for {
        i <- elementsOf(ints)
        m <- elementsOf(mSymbols)
        ist <- elementsOf(
          (ints.x(symbols)) > omega
        )
      } strength(i ⊕ m)(ist) shouldBe m(
        asElement(
          symbols(omega)(h => ist(i ⊕ h))
        )
      )

      continuation.sanityTest3(ints)
      continuation.sanityTest4(ints, symbols)
      continuation.sanityTest5(ints, symbols, I)
      continuation.sanityTest6(I, I)
    }

    "functorial strength is calculated correctly" in {
      val ints = dot(1)
      val symbols = dot(a, b)

      val mInts = continuation(ints).free
      val mSymbols = continuation(symbols).free

      val strength: (
        (
          Int → Symbol
        ) > (
          (Int → TRUTH → TRUTH) →
            (Symbol → TRUTH → TRUTH)
        )
      ) =
        continuation.functorialStrength(
          ints,
          symbols
        )

      strength should have(
        source(ints > symbols),
        target(mInts > mSymbols)
      )

      for {
        i2s: (Int → Symbol) <- elementsOf(ints > symbols)
        mi: (Int → TRUTH → TRUTH) <- elementsOf(mInts)
        s2o: (Symbol → TRUTH) <- elementsOf(symbols > omega)
      } strength(i2s)(mi)(s2o) shouldBe mi(
        asElement(
          ints(omega)(i => s2o(i2s(i)))
        )
      )
    }

    "multiplication (mu) works" in {
      val mu: (
        UNIT → TRUTH → TRUTH → TRUTH → TRUTH
      ) > (
        UNIT → TRUTH → TRUTH
      ) = continuation(I).mu

      val io2iooo: (UNIT → TRUTH) > (UNIT → TRUTH → TRUTH → TRUTH) =
        (I > omega > omega > omega).transpose(I > omega)((x, f) => f(x))

      implicit val forIoooo = I > omega > omega > omega > omega

      for {
        ioooo <- elementsOf(I > omega > omega > omega > omega)
        ioooo_ : RichExponential[UNIT → TRUTH → TRUTH → TRUTH, TRUTH] = ioooo
      } mu(ioooo) shouldBe (ioooo_.o(io2iooo))

      Monad.sanityTest[
        ({ type λ[X <: ~] = X → TRUTH → TRUTH })#λ,
        UNIT
      ](continuation, I)
      // Can't run sanityTest2, would take too long (2 ^ 2 ^ 2 ^ 2 ^ 2 ^ 2 ^ 1)

// TODO: check that M[X] is a monad, via multiplication as structure map. Or is that...
// TODO: ...too hard to calculate / a trivial consequence of our existing laws?
    }

    "defines a valid home algebra structure map" in {
      continuation.home.structure should have(
        source(omega > omega > omega),
        target(omega)
      )

      val id: TRUTH → TRUTH =
        asElement(
          omega.identity
        )

      for (oo <- elementsOf(omega > omega > omega))
        continuation.home.structure(
          oo
        ) shouldBe oo(id)

      continuation.home.sanityTest
    }
  }
}

package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.language.reflectiveCalls

class ElementEnrichmentTest extends FreeSpec {
  "Exponential element anrichments" - {
    "allow internal composition" in {
      val symbols = dot('a, 'b)
      val ints = dot(1, 2, 3)
      val bools = dot(true, false)

      // TODO: Bake these in deeper
      implicit val xx: EXPONENTIAL[Symbol, Int] = symbols > ints
      implicit val yy: EXPONENTIAL[Int, Boolean] = ints > bools

      for {
        f : (Symbol → Int) <- elementsOf(symbols > ints)
        g: (Int → Boolean) <- elementsOf(ints > bools)
      } {
        val gg = g : RichExponential[Int, Boolean]
        val gof: Symbol → Boolean =
          gg o f

        for {
          s <- elementsOf(symbols)
        }
          gof(s) shouldBe g(f(s))
      }
    }
  }
}

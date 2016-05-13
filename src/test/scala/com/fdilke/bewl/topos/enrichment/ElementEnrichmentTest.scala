package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets._
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.language.{postfixOps, implicitConversions}

class ElementEnrichmentTest extends FreeSpec {
  "Exponential element enrichments" - {
    "allow internal composition" in {
      val symbols = dot('a, 'b)
      val ints = dot(1, 2, 3)
      val bools = dot(true, false)

      object FancyImplicits {

        implicit class DownAtHeelExponential(
          exp: Int → Boolean
        ) {
          def seeOn(t: Boolean) = "eight"
        }

        implicit class UpwardlyMobileExponential[
          S <: ~
        ](
          exp: FiniteSets.→[S, Boolean]
        ) {
          def forOn = -1
        }

        implicit class WellOffExponential[
          S <: ~,
          T <: ~
        ](
          exp: S → T
        ) {
          def doOn(s: String) = 5
        }

        implicit class RichExponential[
          S <: ~,
          T <: ~
        ](
          exp: S → T
        )(
          implicit expDot: EXPONENTIAL[S, T]
        ) {
          def goOn[
            R <: ~
          ](
            preExp: R → S
          )(
            implicit preExpDot: EXPONENTIAL[R, S]
          ): R → T = (
            preExpDot.source > expDot.target
            ).transpose(
            preExpDot
          ) {
            (sr, r) => exp(sr(r))
          }(
            preExp
          )

          def beOn(n: Int) = "hehh"
        }

        implicit class RichSkinflint[S](
          s: S
        )(
          implicit metric: S => Int
        ) {
          def measure = metric(s)
        }
      }
      import FancyImplicits._

      implicit val stringMetric: String => Int =
        _.length

      "Felix".measure shouldBe 5

      // TODO: Bake these in deeper
      implicit val xx: EXPONENTIAL[Symbol, Int] =
        symbols > ints

      implicit val yy: EXPONENTIAL[Int, Boolean] =
        ints > bools

      for {
        f : (Symbol → Int) <- elementsOf(symbols > ints)
        g: (Int → Boolean) <- elementsOf(ints > bools)
      } {
        val h = (g: RichExponential[Int, Boolean]).goOn(f)

        (g seeOn true) shouldBe "eight"
//        (g forOn) shouldBe -1
//        g doOn "hhh"
//        g beOn 2

//        val gof: Symbol → Boolean =
//          g.goOn(f)
//
//        for {
//          s <- elementsOf(symbols)
//        }
//          gof(s) shouldBe g(f(s))
      }
    }
  }
}

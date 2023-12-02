package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import com.fdilke.bewl2.sets.Sets
import Sets.{
  withDot,
  Monoid,
  Dot,
  MonoidActionGeneratorFinder,
  PresentationFinder
}
import com.fdilke.bewl2.utility.StockStructures._

import scala.language.{postfixOps, reflectiveCalls}

class FindMonoidActionPresentationSpec extends FunSuite:

  private val Seq(i, x, y) =
    Seq[String]("i", "x", "y").map { Symbol(_) }

  withMonoidOf3(Sets):
    (_: Dot[Symbol]) ?=> (monoidOf3: Sets.Monoid[Symbol]) ?=>

    monoidOf3.withRegularAction:
      (regularAction: monoidOf3.Action[Symbol]) ?=>

      val actionTopos: Topos[
        monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap
      ] =
        monoidOf3.actionTopos

      val scalarMultiply: ((String, Symbol)) => String =
        (s, m) => monoidOf3.multiply(Symbol(s), m).name

      val omega: monoidOf3.Action[monoidOf3.RightIdeal] =
        actionTopos.pretopos.omegaDot

      def canExtractPresentation[A](action: monoidOf3.Action[A]) =
        CheckExtractPresentation(monoidOf3)(action)

      withDot(Set[String]("x", "y")):
        (barDot: Dot[String]) ?=>

        monoidOf3.withAction(scalarMultiply):
          (bar: monoidOf3.Action[String]) ?=>

          test("Can extract a presentation for the regular monoid action"):
            canExtractPresentation(regularAction)

          test("Can extract a presentation for an empty monoid action"):
            monoidOf3.withVoidAction:
              (voidAction: monoidOf3.Action[Void]) ?=>
                canExtractPresentation(voidAction)
      
          test("Can extract a presentation for a sample action"):
            canExtractPresentation(bar)

          test("Can extract a presentation for a sample action squared"):
            canExtractPresentation(bar.x(bar))

          test("Can extract a presentation for the truth object monoid action"):
            canExtractPresentation(omega)

          test("Can extract a presentation for a more fancy monoid action"):
            canExtractPresentation(omega x regularAction)



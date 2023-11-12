package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import com.fdilke.bewl2.sets.{ FastSets }
import FastSets.{
  withDot,
  Monoid,
  Dot,
  FindGeneratorAnalysis,
  GeneratorFinder,
  PresentationFinder,
}
import com.fdilke.bewl2.utility.StockStructures._

import scala.language.{postfixOps, reflectiveCalls}

class FindGeneratorsTest extends FunSuite:

  private val Seq(i, x, y) =
    Seq[String]("i", "x", "y").map { Symbol(_) }

  withMonoidOf3(FastSets):
    (_: Dot[Symbol]) ?=> (monoidOf3: FastSets.Monoid[Symbol]) ?=>

    monoidOf3.withRegularAction:
      (regularAction: monoidOf3.Action[Symbol]) ?=>

      val generatorFinder: GeneratorFinder[Symbol, monoidOf3.Action] =
        GeneratorFinder.forMonoid(monoidOf3)
      val presentationFinder: PresentationFinder[Symbol, monoidOf3.Action] =
        PresentationFinder.forMonoid(monoidOf3, generatorFinder)

      val regularAnalysis: FindGeneratorAnalysis[Symbol, Symbol] =
        generatorFinder.findGenerators(regularAction)

      val actionTopos: Topos[
        monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap
      ] =
        monoidOf3.actionTopos

      import regularAnalysis.initialCyclics
      
      test("Action analysis gives maximal cyclic subalgebras which are initially empty"):
        initialCyclics.cyclics.isEmpty is true
        initialCyclics.contains(i) is false
        initialCyclics.transversal.isEmpty is true

      test("MCSs can be added to, filtering out any eclipsed cyclics"):
        val cyclics_I: AbstractCyclics[Symbol] =
          initialCyclics + i

        cyclics_I.cyclics.size is 1

        val cyclics_X_Y: AbstractCyclics[Symbol] =
          initialCyclics + x + y

        cyclics_X_Y.cyclics.size is 1

        val theCyclics: Seq[AbstractCyclic[Symbol]] =
          (cyclics_X_Y + i).cyclics

        theCyclics.size is 1
        theCyclics.head.generator is i

      test("MCSs can be used to build up the complete set"):
        val allMaxCyclics: AbstractCyclics[Symbol] =
          Seq(i, x, y).foldLeft(
            initialCyclics
          ) { _ << _ }

        val theCyclics: Seq[AbstractCyclic[Symbol]] =
          allMaxCyclics.cyclics

        theCyclics.size is 1
        theCyclics.head.generator is i

      test("MCSs are as expected for the void action"):
        monoidOf3.withVoidAction:
          (voidAction: monoidOf3.Action[Void]) ?=>
          generatorFinder.findGenerators(voidAction).generators.isEmpty is true

      test("MCSs are as expected for a non-cyclic action"):
        val regularSquared: monoidOf3.Action[(Symbol, Symbol)] = 
          regularAction x regularAction
        generatorFinder.findGenerators(
          regularSquared
        ).generators.size is 7

      test("MCSs are as expected for another non-cyclic action"):
        val omega: monoidOf3.Action[monoidOf3.RightIdeal] =
          actionTopos.pretopos.omegaDot
        generatorFinder.findGenerators(omega).generators.size is 2
      
      test("MCSs can extract a set of generators for a monoid action"):
        generatorFinder.findGenerators(regularAction).generators is Seq(i)


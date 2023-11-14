package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import com.fdilke.bewl2.sets.Sets
import Sets.{
  withDot,
  Monoid,
  Dot,
  FindGeneratorAnalysis,
  GeneratorObtainer,
  PresentationFinder
}
import com.fdilke.bewl2.utility.StockStructures._

import scala.language.{postfixOps, reflectiveCalls}

class ObtainGeneratorsSpec extends FunSuite:
  private val Seq(i, x, y) =
    Seq[String]("i", "x", "y").map { Symbol(_) }

  withMonoidOf3(Sets):
    (_: Dot[Symbol]) ?=> (monoidOf3: Sets.Monoid[Symbol]) ?=>
    val generatorFinder: GeneratorObtainer[Symbol, monoidOf3.Action] =
      GeneratorObtainer.forMonoid(monoidOf3)

    val actionTopos: Topos[
      monoidOf3.Action, [A] =>> A, Void, Unit, monoidOf3.RightIdeal, monoidOf3.InternalMap
    ] =
      monoidOf3.actionTopos

    test("Generators are as expected for the regular action"):
      monoidOf3.withRegularAction:
        (regularAction: monoidOf3.Action[Symbol]) ?=>
        val generators: Seq[Symbol] =
          generatorFinder.findGenerators(regularAction)
        generators is Seq(i)

    test("Generators are as expected for the void action"):
      monoidOf3.withVoidAction:
        (voidAction: monoidOf3.Action[Void]) ?=>
        generatorFinder.findGenerators(voidAction) is Seq()

    test("Generators are as expected for a non-cyclic action"):
      monoidOf3.withRegularAction:
        (regularAction: monoidOf3.Action[Symbol]) ?=>
        val regularSquared: monoidOf3.Action[(Symbol, Symbol)] = 
          regularAction x regularAction
        generatorFinder.findGenerators(
          regularSquared
        ).size is 7

    test("Generators are as expected for another non-cyclic action"):
      val omega: monoidOf3.Action[monoidOf3.RightIdeal] =
        actionTopos.pretopos.omegaDot
      generatorFinder.findGenerators(omega).size is 2
    





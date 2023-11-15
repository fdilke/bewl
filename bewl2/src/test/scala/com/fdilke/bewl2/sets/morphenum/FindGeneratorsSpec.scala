package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos

import com.fdilke.bewl2.sets.Sets
import Sets.{
  withDot,
  Monoid,
  Dot,
  GeneratorFinder,
  PresentationFinder
}
import com.fdilke.bewl2.utility.StockStructures._

import scala.language.{postfixOps, reflectiveCalls}

class FindGeneratorsSpec extends FunSuite:
  private val Seq(i, x, y) =
    Seq[String]("i", "x", "y").map { Symbol(_) }

  withMonoidOf3(Sets):
    (monoidDot: Dot[Symbol]) ?=> (monoidOf3: Sets.Monoid[Symbol]) ?=>
    val monoidElements: Set[Symbol] = monoidDot.dot
    val generatorFinder: GeneratorFinder[Symbol, monoidOf3.Action] =
      GeneratorFinder.forMonoid(monoidOf3)

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
        checkMinimalGeneratingSet(regularAction)(generators)

    test("Generators are as expected for the void action"):
      monoidOf3.withVoidAction:
        (voidAction: monoidOf3.Action[Void]) ?=>
        generatorFinder.findGenerators(voidAction) is Seq()
        checkMinimalGeneratingSet(voidAction)()

    test("Generators are as expected for a non-cyclic action"):
      monoidOf3.withRegularAction:
        (regularAction: monoidOf3.Action[Symbol]) ?=>
        val regularSquared: monoidOf3.Action[(Symbol, Symbol)] = 
          regularAction x regularAction
        val generators: Seq[(Symbol, Symbol)] = 
          generatorFinder.findGenerators(regularSquared)
        generators.size is 7
        checkMinimalGeneratingSet(regularSquared)(generators)        

    test("Generators are as expected for another non-cyclic action"):
      val omega: monoidOf3.Action[monoidOf3.RightIdeal] =
        actionTopos.pretopos.omegaDot
      val generators: Seq[monoidOf3.RightIdeal] = 
        generatorFinder.findGenerators(omega)
      generators.size is 2
      checkMinimalGeneratingSet(omega)(generators)

    test("Generators are as expected for other more complex actions"):
      monoidOf3.withRegularAction:
        (regularAction: monoidOf3.Action[Symbol]) ?=>
        val omega: monoidOf3.Action[monoidOf3.RightIdeal] =
          actionTopos.pretopos.omegaDot
        given Dot[monoidOf3.RightIdeal] = omega.dot
        checkMinimalGeneratingSet(regularAction x omega)()
        checkMinimalGeneratingSet(regularAction x omega x regularAction)()
        checkMinimalGeneratingSet(regularAction x omega x regularAction x omega)()

    def checkMinimalGeneratingSet[A](
      action: monoidOf3.Action[A]
    )(
      generators: Seq[A] = generatorFinder.findGenerators(action)
    ): Unit =
      val actionElements: Set[A] = action.dot.dot
      def generatedBy(elements: Seq[A]): Set[A] =
        for 
          m <- monoidElements
          e <- elements.toSet
        yield 
          action.actionMultiply(e, m)

      generatedBy(generators) is actionElements
      for { g <- generators }
        generatedBy(generators.filter { _ != g}) isnt actionElements
    





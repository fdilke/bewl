package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.utility.StockStructures._

import com.fdilke.bewl2.sets.Sets
import Sets.{
  withDot,
  Group,
  Dot,
  GroupActionGeneratorFinder
}

abstract class FindGroupActionGeneratorsSpec extends FunSuite:
  private val Seq(e, a, b, c, r, s) =
    Seq[String]("e", "a", "b", "c", "r", "s").map { Symbol(_) }

  with_S_3:
    (groupDot: Dot[Symbol]) ?=> (s3: Group[Symbol]) ?=>
    val groupElements: Set[Symbol] = groupDot.dot
    val generatorFinder: GroupActionGeneratorFinder[s3.Action] =
      GroupActionGeneratorFinder.forGroup(s3)

    val actionTopos: Topos[
      s3.Action, [A] =>> A, Void, Unit, Boolean, Map
    ] =
      s3.actionTopos

    test("Generators are as expected for the regular action"):
      s3.withRegularAction:
        (regularAction: s3.Action[Symbol]) ?=>
        val generators: Seq[Symbol] =
          generatorFinder(regularAction)
        generators.size is 1
        checkMinimalGeneratingSet(regularAction)(generators)

    test("Generators are as expected for the void action"):
      s3.withVoidAction:
        (voidAction: s3.Action[Void]) ?=>
        generatorFinder(voidAction) is Seq()
        checkMinimalGeneratingSet(voidAction)()

    test("Generators are as expected for a non-cyclic action"):
      s3.withRegularAction:
        (regularAction: s3.Action[Symbol]) ?=>
        val regularSquared: s3.Action[(Symbol, Symbol)] = 
          regularAction x regularAction
        val generators: Seq[(Symbol, Symbol)] = 
          generatorFinder(regularSquared)
        generators.size is 7 // actually 6?
        checkMinimalGeneratingSet(regularSquared)(generators)        

    test("Generators are as expected for another non-cyclic action"):
      val omega: s3.Action[Boolean] =
        actionTopos.pretopos.omegaDot
      val generators: Seq[Boolean] = 
        generatorFinder(omega)
      generators.size is 2
      checkMinimalGeneratingSet(omega)(generators)

    test("Generators are as expected for other more complex actions"):
      s3.withRegularAction:
        (regularAction: s3.Action[Symbol]) ?=>
        val omega: s3.Action[Boolean] =
          actionTopos.pretopos.omegaDot
        given Dot[Boolean] = omega.dot
        checkMinimalGeneratingSet(regularAction x omega)()
        checkMinimalGeneratingSet(regularAction x omega x regularAction)()
        checkMinimalGeneratingSet(regularAction x omega x regularAction x omega)()

    def checkMinimalGeneratingSet[A](
      action: s3.Action[A]
    )(
      generators: Seq[A] = generatorFinder(action)
    ): Unit =
      val actionElements: Set[A] = action.dot.dot
      def generatedBy(elements: Seq[A]): Set[A] =
        for 
          g <- groupElements
          x <- elements.toSet
        yield 
          action.actionMultiply(x, g)

      generatedBy(generators) is actionElements
      for { x <- generators }
        generatedBy(generators.filter { _ != x}) isnt actionElements
    

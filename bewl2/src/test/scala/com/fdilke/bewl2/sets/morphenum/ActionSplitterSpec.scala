package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.Sets
import Sets.{ ActionComponent, ActionSplitter, MonoidActionGeneratorFinder, PresentationFinder }

import com.fdilke.bewl2.utility.StockStructures._
import scala.language.{postfixOps, reflectiveCalls}

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import com.fdilke.bewl2.helper.BuildEquivalence
import scala.language.{existentials, reflectiveCalls}

class ActionSplitterSpec extends FunSuite:

  withMonoidOf3(Sets):
    (_: Sets.Dot[Symbol]) ?=> (monoidOf3: Sets.Monoid[Symbol]) ?=>

      monoidOf3.withRegularAction:
        (regularAction: monoidOf3.Action[Symbol]) ?=>

        val generatorFinder: MonoidActionGeneratorFinder[monoidOf3.Action] =
          MonoidActionGeneratorFinder.forMonoid(monoidOf3)
        val presentationFinder: PresentationFinder[Symbol, monoidOf3.Action] =
          PresentationFinder.forMonoid(monoidOf3, generatorFinder)

        val splitter: ActionSplitter[
          Symbol,
          ({ type λ[T] = monoidOf3.Action[T] })#λ
        ] = ActionSplitter.forMonoid(monoidOf3, generatorFinder, presentationFinder)

        def components[A](action: monoidOf3.Action[A]): Seq[
          ActionComponent[Symbol, A, monoidOf3.Action]
        ] =
          splitter.splitAction(action).components

        Sets.withDot(Set[String]("x", "y")):
          test("Action splitter extracts coproduct decomposition for the empty monoid action"):
            monoidOf3.withVoidAction:
              (voidAction: monoidOf3.Action[Void]) ?=> 
                components(voidAction).isEmpty is true

          test("and for the regular monoid action"):
            val regularSplitting: Seq[ActionComponent[Symbol, Symbol, monoidOf3.Action]] =
              components(regularAction)

            regularSplitting.size is 1
            regularSplitting.head.componentAction.dot.dot.size is 3

          test("and for regularAction x bar"):
            val bar: monoidOf3.Action[String] =
              monoidOf3.Action:
                (s, m) => monoidOf3.multiply(Symbol(s), m).name
            val regbarSplitting: Seq[ActionComponent[Symbol, (Symbol, String), monoidOf3.Action]]=
              components(regularAction x bar)

            regbarSplitting.size is 1
            regbarSplitting.head.componentAction.dot.dot.size is 6


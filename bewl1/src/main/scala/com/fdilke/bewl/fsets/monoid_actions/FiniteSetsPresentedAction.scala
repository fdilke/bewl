package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.BuildEquivalence
import scala.language.postfixOps
import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{>, makeDot}

object FiniteSetsPresentedAction {
  def apply[M, A](
    monoid: FiniteSets.Monoid[M]
  )(
    generatorsWithRelators: Seq[GeneratorWithRelators[M, A]]
  ): monoid.PresentedAction[Int] = {
    val monoidElements: List[M] =
      elementsOf(monoid.carrier).toList
    val lookupMonoid: Map[M, Int] =
      monoidElements.zipWithIndex.toMap
    val generators: List[A] =
      generatorsWithRelators.map(_.generator) toList
    val lookupGenerator: Map[A, Int] =
      generators.zipWithIndex.toMap

    val words = makeDot(generators).x(monoid.carrier)

    def indexOfWord(g: A, m: M): Int =
      lookupGenerator(g) * monoidElements.size + lookupMonoid(m)

    def wordOfIndex(index: Int): (A, M) =
      generators(index / monoidElements.size) ->
        monoidElements(index % monoidElements.size)

    val equivalenceTable: Seq[Int] =
      BuildEquivalence(
        generators.size * monoidElements.size,
        for {
          gr <- generatorsWithRelators
          g = gr.generator
          Relator(m, index, n) <- gr.relators
          p <- monoidElements
        } yield (
          indexOfWord(g, monoid.multiply(m, p)),
          indexOfWord(generators(index), monoid.multiply(n, p))
        )
      )

    val wordIndices = equivalenceTable.toSet
    val wordIndicesDot = makeDot(wordIndices)

    new monoid.PresentedAction[Int] {
      override val action: monoid.Action[Int] =
        monoid.action(wordIndicesDot) { (index, n) =>
          val (g, m) = wordOfIndex(index)
          val mn = monoid.multiply(m, n)
          val product = equivalenceTable(
            indexOfWord(g, mn)
          )
          product
        }
      override def project[B](
        otherAction: monoid.Action[B],
        targetElements: Seq[B]
      ): Int > B =
        wordIndicesDot(otherAction.actionCarrier) { index =>
          val (g, m) = wordOfIndex(index)
          otherAction.actionMultiply(
            targetElements(lookupGenerator(g)),
            m
          )
        }
      override def sanityTest: Unit = {
        for {
          (gr, i) <- generatorsWithRelators.zipWithIndex
          relator <- gr.relators
        } assert(relator.otherIndex <= i)

        action.sanityTest
      }
    }
  }
}

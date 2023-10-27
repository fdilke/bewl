package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.SetsUtilities._
import com.fdilke.bewl2.helper.BuildEquivalence
import scala.language.postfixOps
import com.fdilke.bewl2.sets.FastSets

trait PresentedAction[A, ACTION[_]]:
  val action: ACTION[A]
  def project[B](
    otherAction: ACTION[B],
    targetElements: Seq[B]
  ): A => B
  def sanityTest: Unit

object FiniteSetsPresentedAction:
  def apply[M, A](
    monoid: FastSets.Monoid[M]
  )(
    generatorsWithRelators: Seq[GeneratorWithRelators[M, A]]
  ): PresentedAction[Int, monoid.Action] =
    val monoidElements: List[M] =
      monoid.dot.dot.toList
    val lookupMonoid: Map[M, Int] =
      monoidElements.zipWithIndex.toMap
    val generators: List[A] =
      generatorsWithRelators.map(_.generator) toList
    val lookupGenerator: Map[A, Int] =
      generators.zipWithIndex.toMap

    // val words = makeDot(generators).x(monoid.carrier)

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

    val wordIndices: Set[Int] = equivalenceTable.toSet
    FastSets.withDot(wordIndices):
      new PresentedAction[Int, monoid.Action]:
        override val action: monoid.Action[Int] =
          monoid.Action{ (index, n) =>
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
        ): Int => B = index =>
          val (g, m) = wordOfIndex(index)
          otherAction.actionMultiply(
            targetElements(lookupGenerator(g)),
            m
          )
        override def sanityTest: Unit =
          for
            (gr, i) <- generatorsWithRelators.zipWithIndex
            relator <- gr.relators
          do
            assert(relator.otherIndex <= i)
          action.sanityTest
    

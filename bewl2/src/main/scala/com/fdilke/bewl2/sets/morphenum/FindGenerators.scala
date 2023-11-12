package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.{BaseSets, Sets}

import scala.language.postfixOps

trait AbstractCyclic[A] {
  val generator: A
}

trait AbstractCyclics[A] {
  val cyclics: Seq[AbstractCyclic[A]]
  lazy val transversal: Seq[A]

  def contains(a: A): Boolean
  def +(a: A): AbstractCyclics[A]
  def <<(a: A): AbstractCyclics[A]
}

trait FindGenerators extends BaseSets {

  trait FindGeneratorAnalysis[M, A] {
    lazy val initialCyclics: AbstractCyclics[A]
    lazy val generators: Seq[A]
  }

  trait GeneratorFinder[M, ACTION[_]]:
    def findGenerators[A](action: ACTION[A]): FindGeneratorAnalysis[M, A]

  object GeneratorFinder {
    def forMonoid[M](
      monoid: Monoid[M]
    ): GeneratorFinder[M, monoid.Action] =
      new GeneratorFinder[M, monoid.Action]:
        private val monoidElements: Set[M] =
          monoid.dot.dot

        def findGenerators[A](action: monoid.Action[A]): FindGeneratorAnalysis[M, A] =
          new FindGeneratorAnalysis[M, A]:
            private val actionElements: Set[A] =
              action.dot.dot

            case class Cyclic(
              override val generator: A
            ) extends AbstractCyclic[A] {
              val elements: Set[A] =
                monoidElements.map { (m: M) =>
                  action.actionMultiply(
                    generator,
                    m
                  )
                } toSet

              def contains(a: A): Boolean =
                elements contains a

              def subsetOf(
                other: Cyclic
              ): Boolean =
                elements.subsetOf(other.elements)
            }

            class MaximalCyclics(
              override val cyclics: Seq[Cyclic] = Seq.empty
            ) extends AbstractCyclics[A] { self =>

              override def contains(a: A) =
                cyclics.exists {
                  _ contains a
                }

              private def +(
                newCyclic: Cyclic
              ) =
                new MaximalCyclics(
                  newCyclic +:
                    cyclics.filterNot {
                      _.subsetOf(newCyclic)
                    }
                )

              override def +(
                a: A
              ): MaximalCyclics =
                self + Cyclic(a)

              override lazy val transversal =
                cyclics.map {
                  _.generator
                }

              override def <<(a: A): MaximalCyclics =
                if (self contains a)
                  self
                else
                  self + Cyclic(a)
            }

            override lazy val generators =
              actionElements
                .foldLeft(
                  initialCyclics
                ) {
                  _ << _
                } transversal

            override lazy val initialCyclics =
              new MaximalCyclics
          }
      }


package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.{BaseFiniteSets, FiniteSets}

import scala.language.postfixOps

trait AbstractCyclic[A] {
  val generator: A
}

trait AbstractCyclics[A] {
  val cyclics: Seq[AbstractCyclic[A]]
  val transversal: Seq[A]

  def contains(a: A): Boolean
  def +(a: A): AbstractCyclics[A]
  def <<(a: A): AbstractCyclics[A]
}

trait FindGenerators extends BaseFiniteSets {

  trait FindGeneratorAnalysis[M, A] {
    val initialCyclics: AbstractCyclics[A]
    val generators: Seq[A]
  }

  object FindGenerators {
    def forMonoid[M](
      monoid: Monoid[M]
    ): {
      def apply[A](
        action: monoid.Action[A]
      ): FindGeneratorAnalysis[M, A]
    } =
      new Object {
        private val monoidElements =
          monoid.carrier.elements

        def apply[A](action: monoid.Action[A]) =
          new FindGeneratorAnalysis[M, A] {
            private val actionElements =
              action.carrier.elements

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
  }
}

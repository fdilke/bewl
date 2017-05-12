package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import scala.language.higherKinds
import scala.language.postfixOps
import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.helper.⊕
import ⊕._
import FiniteSets.{ >, x }
import scala.Function.tupled

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

trait AbstractActionAnalysis[M, A] {
  val initialCyclics: AbstractCyclics[A]
  val generators: Seq[A]
  val presentation: Seq[GeneratorWithRelators[M, A]]
}

object FiniteSetsMonoidAction {
  def apply[M](
    monoid: FiniteSets.Monoid[M]) = {
    val monoidElements =
      elementsOf(monoid.carrier)

    new monoid.ActionAnalyzer[({
      type λ[X] = AbstractActionAnalysis[M, X] with monoid.MorphismEnumerator[X]
    })#λ] {
      override def analyze[A](
        action: monoid.Action[A]) = new AbstractActionAnalysis[M, A] with monoid.MorphismEnumerator[A] {

        case class Cyclic(
            override val generator: A) extends AbstractCyclic[A] {
          val elements: Set[A] =
            monoidElements map { (m: M) =>
              action.actionMultiply(
                generator,
                m)
            } toSet

          def contains(a: A) =
            elements contains a

          def subsetOf(
            other: Cyclic) =
            elements.subsetOf(
              other.elements)
        }

        class MaximalCyclics(
            override val cyclics: Seq[Cyclic] = Seq.empty) extends AbstractCyclics[A] { self =>

          override def contains(a: A) =
            cyclics.exists {
              _ contains a
            }

          private def +(
            newCyclic: Cyclic) =
            new MaximalCyclics(
              newCyclic +: (
                cyclics filterNot {
                  _ subsetOf newCyclic
                }))

          override def +(
            a: A): MaximalCyclics =
            self + Cyclic(a)

          override lazy val transversal =
            cyclics map {
              _.generator
            }

          override def <<(a: A): MaximalCyclics =
            if (self contains a)
              self
            else
              self + Cyclic(a)
        }

        private val actionElements =
          elementsOf(action.carrier)

        override lazy val initialCyclics =
          new MaximalCyclics

        override lazy val generators =
          actionElements.foldLeft(
            initialCyclics) {
              _ << _
            }.transversal

        override lazy val presentation: Seq[GeneratorWithRelators[M, A]] =
          generators.zipWithIndex map tupled { (g, j) =>
            GeneratorWithRelators[M, A](
              g,
              generators.take(j + 1).zipWithIndex
                flatMap tupled { (f, i) =>
                  analyze(
                    monoid.action(
                      (monoid.carrier x monoid.carrier) where {
                        case m ⊕ n =>
                          action.actionMultiply(g, m) ==
                            action.actionMultiply(f, n)
                      }) {
                        (pair, m) =>
                          pair match {
                            case p ⊕ q =>
                              monoid.multiply(p, m) ⊕
                                monoid.multiply(q, m)
                          }
                      }).generators map {
                      case m ⊕ n =>
                        Relator(m, 0, n)
                    }
                })
          }

        override def morphismsTo[B](
          target: monoid.Action[B]) =
          new Traversable[A > B] {
            override def foreach[U](
              f: (A > B) => U) {
              ???
            }
          }
      }
    }
  }
}


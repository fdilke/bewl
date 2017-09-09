package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.BaseFiniteSets
import com.fdilke.bewl.helper.⊕
import ⊕._
import scala.Function.tupled
import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait PresentationFinder extends BaseFiniteSets {
  Ɛ: GeneratorFinder =>

  object PresentationFinder {
    def forMonoid[M](
      monoid: Monoid[M]
    ): {
      def findPresentation[A](
        action: monoid.Action[A]
      ): Seq[GeneratorWithRelators[M, A]]
    } =
      new Object {
        private val monoidElements =
          monoid.carrier.elements

        private val generatorFinder: {
          def findGenerators[A](
            action: monoid.Action[A]
          ): FindGeneratorAnalysis[M, A]
        } =
          GeneratorFinder.forMonoid(
            monoid
          )

        def findPresentation[A](action: monoid.Action[A]) = {
          val generators =
          generatorFinder findGenerators(
            action
            ) generators

          generators.zipWithIndex map tupled { (g, j) =>
            GeneratorWithRelators[M, A](
              g,
              generators.take(j + 1).zipWithIndex
                flatMap tupled { (h, i) =>
                generatorFinder.findGenerators(
                  monoid.action(
                    monoid.carrier.squared where {
                      case m ⊕ n =>
                        action.actionMultiply(g, m) ==
                          action.actionMultiply(h, n)
                    }) {
                    (pair, m) =>
                      pair match {
                        case p ⊕ q =>
                          monoid.multiply(p, m) ⊕
                            monoid.multiply(q, m)
                      }
                  }
                ).generators collect {
                  case m ⊕ n if !(m == n && i == j) =>
                    Relator(m, i, n)
                }
              })
          }
        }
      }
  }
}

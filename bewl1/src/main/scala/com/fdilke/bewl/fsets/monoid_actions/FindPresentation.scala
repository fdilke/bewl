package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.BaseFiniteSets
import com.fdilke.bewl.helper.⊕
import ⊕._
import scala.Function.tupled
import scala.language.{postfixOps, reflectiveCalls}

trait FindPresentation extends BaseFiniteSets {
  Ɛ: FindGenerators =>

  object FindPresentation {
    def forMonoid[M](
      monoid: Monoid[M]
    ): {
      def apply[A](
        action: monoid.Action[A],
        generators: Seq[A]
      ): Seq[GeneratorWithRelators[M, A]]
    } =
      new Object {
        private val findGenerators: {
          def apply[A](
            action: monoid.Action[A]
          ): FindGeneratorAnalysis[M, A]
        } =
          FindGenerators.forMonoid(
            monoid
          )

        def apply[A](
          action: monoid.Action[A],
          generators: Seq[A]
        ) =
          generators.zipWithIndex.map(tupled { (g, j) =>
            GeneratorWithRelators[M, A](
              g,
              CullRelators(
                j,
                generators
                  .take(j + 1)
                  .zipWithIndex
                  .flatMap(tupled { (h, i) =>
                    findGenerators(
                      monoid.action(
                        monoid.carrier.squared.where {
                          case m ⊕ n =>
                            action.actionMultiply(g, m) ==
                              action.actionMultiply(h, n)
                        }
                      ) { (pair, m) =>
                        pair match {
                          case p ⊕ q =>
                            monoid.multiply(p, m) ⊕
                              monoid.multiply(q, m)
                        }
                      }
                    ).generators.map {
                      case m ⊕ n =>
                        Relator(m, i, n)
                    }
                  })
              )
            )
          })
      }
  }
}

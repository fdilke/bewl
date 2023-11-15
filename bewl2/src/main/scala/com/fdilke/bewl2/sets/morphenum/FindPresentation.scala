package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets
import scala.Function.tupled
import scala.language.{postfixOps, reflectiveCalls}

trait FindPresentation extends BaseSets:
  Ɛ: FindGenerators =>

  trait PresentationFinder[M, ACTION[_]]:
      def findPresentation[A](action: ACTION[A], generators: Seq[A]): Seq[GeneratorWithRelators[M, A]]

  object PresentationFinder:
    def forMonoid[M](
      monoid: Monoid[M],
      generatorFinder: GeneratorFinder[M, monoid.Action]
    ): PresentationFinder[M, monoid.Action] =
      new PresentationFinder[M, monoid.Action]:
        private def specialGenerators[A](
          action: monoid.Action[A], 
          g: A, 
          h: A
        ): Seq[(M, M)] =
          val specialActionSet: Set[(M, M)] =
            (for { 
              m <- monoid.dot.dot
              n <- monoid.dot.dot if (
                action.actionMultiply(g, m) ==
                  action.actionMultiply(h, n)
              )
            } yield (m, n)).toSet
          val specialAction: monoid.Action[(M, M)] =
            withDot(specialActionSet) {
              given Dot[M] = monoid.dot
              monoid.Action[(M, M)] { (pair, m) =>
                pair match {
                  case p ⊕ q =>
                    monoid.multiply(p, m) -> monoid.multiply(q, m)
                }
              }
            }            
          generatorFinder.findGenerators(specialAction)

        override def findPresentation[A](action: monoid.Action[A], generators: Seq[A]): Seq[GeneratorWithRelators[M, A]] =
          generators.zipWithIndex.map(tupled { (g, j) =>
            GeneratorWithRelators[M, A](
              g,
              CullRelators(
                j,
                generators
                  .take(j + 1)
                  .zipWithIndex
                  .flatMap(tupled { (h, i) =>
                    specialGenerators(action, g, h).map {
                      case (m, n) =>
                        Relator(m, i, n)
                    }
                  })
              )
            )
          })
        
          
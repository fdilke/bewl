package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.BaseFiniteSets
import com.fdilke.bewl.helper.Memoize

import scala.Function.tupled
import scala.language.{postfixOps, reflectiveCalls}

trait FiniteSetsMonoidAssistant extends BaseFiniteSets {
  Ɛ: FindGenerators with FindPresentation with ActionSplitter =>

  object LocalMonoidAssistant extends MonoidAssistant {
    override def actionAnalyzer[
      M <: ~
    ](
      monoid: Monoid[M]
    ): monoid.ActionAnalyzer =
      new monoid.ActionAnalyzer {
        abstract class FiniteSetsActionAnalysis[A <: ~](
          override val action: monoid.Action[A]
        ) extends monoid.ActionAnalysis[A, FiniteSetsActionAnalysis] {
          val actionSplitting: ActionSplitting[
            M,
            A,
            ({ type λ[X <: ~] = monoid.Action[X] })#λ
          ]
        }

        override type ANALYSIS[A <: ~] = FiniteSetsActionAnalysis[A]

        private val findPresentation: {
          def apply[A](
            action: monoid.Action[A],
            generators: Seq[A]
          ): Seq[GeneratorWithRelators[M, A]]
        } =
          FindPresentation.forMonoid(
            monoid
          )

        private val findGenerators: {
          def apply[A](
            action: monoid.Action[A]
          ): FindGeneratorAnalysis[M, A]
        } =
          FindGenerators.forMonoid(
            monoid
          )

        private val actionSplitter: ActionSplitter[
          M,
          ({ type λ[T] = monoid.Action[T] })#λ
        ] =
          ActionSplitter.forMonoid(
            monoid
          )

        private val monoidElements =
          monoid.carrier.elements

        override def analyze[A <: ~](
          action: monoid.Action[A]
        ) =
          new FiniteSetsActionAnalysis[A](
            action
          ) {
            override val actionSplitting =
              actionSplitter.splitAction(
                action
              )

            import actionSplitting.allGenerators

            private val generatorsWithRelators =
              findPresentation(
                action,
                allGenerators
              )

            private def mapsBetween[A, B](
              src: ActionComponent[M, A, ({ type λ[T] = monoid.Action[T] })#λ],
              tgt: ActionComponent[M, B, ({ type λ[T] = monoid.Action[T] })#λ]
            ): Iterable[Map[A, B]] = {
              val srcGenerators =
                src.componentGenerators
              val srcAction =
                src.componentAction
              val srcPresentation =
                src.componentPresentation

              val targetAction = tgt.componentAction
              val targetCarrier =
                targetAction.actionCarrier
              val targetElements =
                targetCarrier.elements
              val targetMultiply =
                targetAction.actionMultiply

              def compatibleExtensions(
                partialMap: Map[A, B],
                index: Int
              ): Iterable[Map[A, B]] = {
                val gr = srcPresentation(index)
                val generator = gr.generator
                targetElements
                  .filter { targetElement =>
                    gr.relators.forall { relator =>
                      val otherTarget: B =
                        if (relator.otherIndex == index)
                          targetElement
                        else
                          partialMap(
                            srcGenerators(relator.otherIndex)
                          )
                      targetMultiply(
                        targetElement,
                        relator.selfScalar
                      ) == targetMultiply(
                        otherTarget,
                        relator.otherScalar
                      )
                    }
                  }
                  .map { targetElement =>
                    {
                      for {
                        m <- monoidElements
                      } yield {
                        srcAction.actionMultiply(
                          generator,
                          m
                        ) -> targetMultiply(
                          targetElement,
                          m
                        )
                      }
                    } toMap
                  }
                  .toSeq distinct
              }

              srcPresentation.indices.foldLeft(
                Iterable(Map.empty[A, B])
              ) { (partialMaps, index) =>
                for {
                  partialMap <- partialMaps
                  extension <- compatibleExtensions(
                    partialMap,
                    index
                  )
                } yield partialMap ++ extension
              }
            }

            private def oldMorphismsTo[B](
              target: FiniteSetsActionAnalysis[B]
            ) = {
              val targetAction = target.action
              val targetCarrier =
                targetAction.actionCarrier

              mapsBetween(
                ActionComponent[M, A, ({ type λ[T] = monoid.Action[T] })#λ](
                  allGenerators,
                  action,
                  generatorsWithRelators
                ),
                ActionComponent[M, B, ({ type λ[T] = monoid.Action[T] })#λ](
                  target.actionSplitting.allGenerators,
                  targetAction,
                  Seq.empty // don't use 'em
                )
              ).map {
                functionAsArrow(
                  action.actionCarrier,
                  targetCarrier,
                  _
                )
              }
            }

            override def morphismsTo[B](
              target: FiniteSetsActionAnalysis[B]
            ) = {
              val srcComponents =
                actionSplitting.components
              val tgtComponents =
                target.actionSplitting.components
              val targetAction = target.action
              val targetCarrier =
                targetAction.actionCarrier

              val blockMaps =
                Memoize[
                  (Int, Int),
                  Iterable[
                    Map[A, B]
                  ]
                ] {
                  tupled { (i, j) =>
                    mapsBetween(
                      srcComponents(i),
                      tgtComponents(j)
                    )
                  }
                }

              def absorb(
                partialMaps: Iterable[Map[A, B]],
                srcIndex: Int
              ): Iterable[Map[A, B]] =
                for {
                  partialMap <- partialMaps
                  tgtIndex <- tgtComponents.indices
                  continuation <- blockMaps(
                    srcIndex,
                    tgtIndex
                  )
                } yield partialMap ++ continuation

              srcComponents.indices
                .foldLeft(
                  Iterable(
                    Map.empty[A, B]
                  )
                ) {
                  absorb
                }
                .map {
                  functionAsArrow(
                    action.actionCarrier,
                    targetCarrier,
                    _
                  )
                }
            }

            lazy val recursiveImago: FiniteSetsActionAnalysis[
              M x A
            ] =
              analyze(
                monoid.regularAction.x(action)
              )

            override def rawExponential[B <: ~](
              target: FiniteSetsActionAnalysis[B]
            ): monoid.RawExponential[A, B] = {
              val targetCarrier =
                target.action.actionCarrier

              implicit val mXa: BIPRODUCT[M, A] =
                monoid.carrier.x(action.actionCarrier)

              def arrowToMap(arrow: M x A > B): M x A → B =
                mXa.elements.map(m_a => m_a -> arrow(m_a)) toMap

              def mapToArrow(arrow: M x A → B): M x A > B =
                mXa.biArrow(targetCarrier)((m, a) => arrow(m ⊕⊕ a)).arrow

              val morphisms: DOT[M x A → B] =
                makeDot(
                  recursiveImago
                    .morphismsTo(
                      target
                    )
                    .map(arrowToMap)
                )

              new monoid.RawExponential[A, B] {
                override val exponentialAction =
                  monoid.action(
                    morphisms
                  ) { (f, m) =>
                    arrowToMap(
                      mXa.biArrow(targetCarrier)((n, a) => f(monoid.multiply(m, n) ⊕⊕ a)).arrow
                    )
                  }

                override val evaluation =
                  morphisms
                    .x(action.actionCarrier)
                    .biArrow(
                      targetCarrier
                    ) { (f, s) =>
                      f(
                        monoid.unit(
                          action.actionCarrier.toI(s)
                        ) ⊕⊕ s
                      )
                    }

                override def transpose[X <: ~](
                  otherAction: monoid.Action[X],
                  biArrow: BiArrow[X, A, B]
                ) =
                  otherAction.actionCarrier(morphisms) { x =>
                    arrowToMap(
                      mXa
                        .biArrow(targetCarrier) { (m, a) =>
                          biArrow(
                            otherAction.actionMultiply(x, m),
                          a
                        )
                      } arrow
                    )
                  }
              }
            }
          }
      }
  }

  override val monoidAssistant =
    LocalMonoidAssistant
}

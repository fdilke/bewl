package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.monoid_actions.{GeneratorFinder, GeneratorWithRelators, PresentationFinder}

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait FiniteSetsMonoidAssistant extends BaseFiniteSets {
  Ɛ: GeneratorFinder with PresentationFinder =>
  
  object LocalMonoidAssistant extends MonoidAssistant {
    override def actionAnalyzer[
      M <: ~
    ] (
      monoid: Monoid[M]
    ): {
      type ANALYSIS[A <: ~] <: monoid.ActionAnalysis[A, ANALYSIS]
      val analyzer: monoid.ActionAnalyzer[ANALYSIS]
    } =
      new Object {
        abstract class FiniteSetsActionAnalysis[A <: ~](
          override val action: monoid.Action[A]
        ) extends monoid.ActionAnalysis[A, FiniteSetsActionAnalysis]

        type ANALYSIS[A <: ~] = FiniteSetsActionAnalysis[A]

        val analyzer: monoid.ActionAnalyzer[ANALYSIS] =
          new monoid.ActionAnalyzer[FiniteSetsActionAnalysis] {
            private val presentationFinder: {
              def findPresentation[A](
                action: monoid.Action[A]
              ): Seq[GeneratorWithRelators[M, A]]
            } =
              PresentationFinder.forMonoid(
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

            private val actionElements =
              action.carrier.elements

            private lazy val generatorsWithRelators =
              presentationFinder.findPresentation(action)

            private lazy val generators =
              generatorsWithRelators map {
                _.generator
              }

            override def morphismsTo[B](
              target: FiniteSetsActionAnalysis[B]
            ) = {
              val targetAction = target.action
              val targetCarrier =
                targetAction.actionCarrier
              val targetElements =
                targetCarrier.elements
              val targetMultiply =
                targetAction.actionMultiply
              def compatibleExtensions(
                partialMap: Map[A, B],
                index: Int
              ): Traversable[Map[A, B]] = {
                val gr = generatorsWithRelators(index)
                val generator = gr.generator
                targetElements.filter { targetElement =>
                  gr.relators forall { relator =>
                    val otherTarget: B =
                      if (relator.otherIndex == index)
                        targetElement
                      else
                        partialMap(
                          generators(relator.otherIndex)
                        )
                    targetMultiply(
                      targetElement,
                      relator.selfScalar
                    ) == targetMultiply(
                      otherTarget,
                      relator.selfScalar
                    )
                  }
                }.map { targetElement =>
                  {
                    for {
                      m <- monoidElements
                    } yield {
                      action.actionMultiply(
                        generator,
                        m
                      ) -> targetMultiply(
                        targetElement,
                        m
                      )
                    }
                  } toMap
                }.toSeq distinct
              }

              def absorb(
                partialMaps: Traversable[Map[A, B]],
                index: Int
              ) =
                for {
                  partialMap <- partialMaps
                  extension <- compatibleExtensions(partialMap, index)
                } yield {
                  partialMap ++ extension
                }

              generatorsWithRelators.indices.foldLeft(
                Traversable(Map.empty[A, B])
              ) {
                absorb
              } map {
                functionAsArrow(
                  action.actionCarrier,
                  targetCarrier,
                  _
                )
              }
            }

            lazy val recursiveImago =
              analyze(monoid.regularAction x action)

            override def rawExponential[B <: ~](
              target: FiniteSetsActionAnalysis[B]
            ): monoid.RawExponential[A, B] = {
              val targetCarrier =
                target.action.actionCarrier

              implicit val mXa: BIPRODUCT[M, A] =
                monoid.carrier x action.actionCarrier

              def arrowToMap(arrow: M x A > B): M x A → B =
                mXa.elements map {
                  m_a => m_a -> arrow(m_a)
                } toMap

              def mapToArrow(arrow: M x A → B): M x A > B =
                  mXa.biArrow(targetCarrier) {
                    (m, a) => arrow(m ⊕⊕ a)
                  }.arrow

              val morphisms: DOT[M x A → B] =
                makeDot(
                  recursiveImago.morphismsTo(
                    target
                  ) map arrowToMap
                )

              new monoid.RawExponential[A, B] {
                override val exponentialAction =
                  monoid.action(
                    morphisms
                  ) {
                    (f, m) =>
                      arrowToMap(
                        mXa.biArrow(targetCarrier) {
                          (n, a) =>
                            f(monoid.multiply(m, n) ⊕⊕ a)
                        }.arrow
                      )
                  }

                override val evaluation =
                  (morphisms x action.actionCarrier).biArrow(
                    targetCarrier
                  ) {
                    (f, s) =>
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
                  otherAction.actionCarrier(morphisms) {
                    x =>
                      arrowToMap(
                        mXa.biArrow(targetCarrier) {
                          (m, a) =>
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
  }

  override val monoidAssistant =
    LocalMonoidAssistant
}


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
    ) =
      new monoid.ActionAnalyzer {
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
          new monoid.ActionAnalysis[A] {

        private val actionElements =
          action.carrier.elements

        private lazy val generatorsWithRelators =
          presentationFinder.findPresentation(action)

        private lazy val generators =
          generatorsWithRelators map {
            _.generator
          }

        override def morphismsTo[B](
          target: monoid.Action[B]
        ) = {
          val targetElements =
            target.actionCarrier.elements
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
                target.actionMultiply(
                  targetElement, 
                  relator.selfScalar
                ) == target.actionMultiply(
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
                  ) -> target.actionMultiply(
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
              target.actionCarrier,
              _
            )
          }
        }
        
        lazy val recursiveImago =
          analyze(monoid.regularAction x action)
        
        override def rawExponential[B <: ~](
          target: monoid.Action[B] 
        ): monoid.RawExponential[A, B] = {
          implicit val mXa: BIPRODUCT[M, A] =
            monoid.carrier x action.actionCarrier

          def arrowToMap(arrow: M x A > B): M x A → B =
            mXa.elements map {
              m_a => m_a -> arrow(m_a)
            } toMap

          def mapToArrow(arrow: M x A → B): M x A > B = 
              mXa.biArrow(target.actionCarrier) {
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
                     mXa.biArrow(target.actionCarrier) {
                       (n, a) => 
                         f(monoid.multiply(m, n) ⊕⊕ a)
                     }.arrow
                   )
                }
                
              override val evaluation = 
                (morphisms x action.actionCarrier).biArrow(
                    target.actionCarrier
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
                      mXa.biArrow(target.actionCarrier) {
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

  override val monoidAssistant =
    LocalMonoidAssistant
}


package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.FiniteSetsUtilities.elementsOf
import com.fdilke.bewl.fsets.monoid_actions.{GeneratorWithRelators, Relator}
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.helper.⊕._

import scala.Function.tupled
import scala.language.{higherKinds, postfixOps}

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

trait FiniteSetsActionAnalysis[M, A] {
  val initialCyclics: AbstractCyclics[A]
  val generators: Seq[A]
  val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]]
}

trait FiniteSetsMonoidAssistant extends BaseFiniteSets {
  
  object LocalMonoidAssistant extends MonoidAssistant {
    override type ACTION_ANALYSIS[M <: ~, A <: ~] = 
      FiniteSetsActionAnalysis[M, A] 
    
    override def actionAnalyzer[
      M <: ~
    ] (
      monoid: Monoid[M]
    ) =
      new monoid.ActionAnalyzer[
        ({
          type λ[A <: ~] = 
            monoid.MonoidSpecificActionAnalysis[A] with 
              FiniteSetsActionAnalysis[M, A]    
        }) # λ
      ] {
        private val monoidElements =
          monoid.carrier.elements
      
        override def analyze[A <: ~](
          action: monoid.Action[A]
        ) = 
          new FiniteSetsActionAnalysis[M, A] with monoid.MonoidSpecificActionAnalysis[A] {

        case class Cyclic(
            override val generator: A) extends AbstractCyclic[A] {
          val elements: Set[A] =
            monoidElements map { (m: M) =>
              action.actionMultiply(
                generator,
                m
              )
            } toSet

          def contains(a: A) =
            elements contains a

          def subsetOf(
            other: Cyclic) =
            elements.subsetOf(
              other.elements
            )
        }

        class MaximalCyclics(
            override val cyclics: Seq[Cyclic] = Seq.empty
        ) extends AbstractCyclics[A] { self =>

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
                }
              )
            )

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
          action.carrier.elements

        override lazy val initialCyclics =
          new MaximalCyclics

        override lazy val generators =
          actionElements.foldLeft(
            initialCyclics
          ) {
              _ << _
            } transversal

        override lazy val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
          generators.zipWithIndex map tupled { (g, j) =>
            GeneratorWithRelators[M, A](
              g,
              generators.take(j + 1).zipWithIndex
                flatMap tupled { (h, i) =>
                  analyze(
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
            targetElements filter { targetElement =>
              gr.relators forall { relator =>
                val otherTarget: B =
                  if (relator.otherIndex == index)
                    targetElement
                  else
                    partialMap(generators(relator.otherIndex))
                target.actionMultiply(
                  targetElement, 
                  relator.selfScalar
                ) == target.actionMultiply(
                  otherTarget,
                  relator.selfScalar
                )
              }
            } map { targetElement =>
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
            }
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
          (0 until generatorsWithRelators.length).foldLeft(
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

          // TODO write better versions of these helper methods
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
              recursiveImago.morphismsTo(target) map arrowToMap
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


package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import scala.language.higherKinds
import scala.language.postfixOps
import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{ >, x, makeDot }
import com.fdilke.bewl.helper.⊕

object Presentation {
  def apply[M, A](
    monoid: FiniteSets.Monoid[M]
  )(
    generatorsWithRelators: List[GeneratorWithRelators[A]]
  ) : monoid.Presentation[A x M] = {
    val generators: List[A] =
      generatorsWithRelators map { _.generator }
    val lookupGenerator: Map[A, Int] =
      generators.zipWithIndex.toMap
    
    val words = makeDot(generators) x monoid.carrier
    
    new monoid.Presentation[A x M] {
      override val action: monoid.Action[A x M] =
        monoid.action(words) { 
          (gm, n) =>
            gm match {
              case g ⊕ m => 
                words.pair(
                  g,
                  monoid.multiply(m, n)
                )
            }
        }
      override def project[B](
        otherAction: monoid.Action[B],
        targetElements: List[B]
      ): (A x M) > B =
        words(otherAction.actionCarrier) { 
          case g ⊕ m => 
            otherAction.actionMultiply(
              targetElements(lookupGenerator(g)),
              m
            )
      }
    }
  }
}

//    val monoidElements = 
//      elementsOf(monoid.carrier)
      
//    new monoid.ActionAnalyzer[
//      ({type λ[X] = AbstractActionAnalysis[X] with 
//        monoid.MorphismEnumerator[X]}) # λ
//    ] {
//    }


package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import scala.language.higherKinds
import scala.language.postfixOps
import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.>

object Presentation {
  def apply[M, A](
    monoid: FiniteSets.Monoid[M]
  )(
    generatorsWithRelators: List[GeneratorWithRelators[A]]
  ) = new monoid.Presentation[A] {
    override val action: monoid.Action[A] =
      ???
    override def project[B](
      otherAction: monoid.Action[B],
      targetElements: List[B]
    ): A > B =
      ???
  }
}

//    val monoidElements = 
//      elementsOf(monoid.carrier)
      
//    new monoid.ActionAnalyzer[
//      ({type λ[X] = AbstractActionAnalysis[X] with 
//        monoid.MorphismEnumerator[X]}) # λ
//    ] {
//    }


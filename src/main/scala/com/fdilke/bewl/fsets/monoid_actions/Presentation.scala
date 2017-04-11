package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.BuildEquivalence
import scala.language.higherKinds
import scala.language.postfixOps
import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{ >, x, makeDot }
import com.fdilke.bewl.helper.⊕

object Presentation {
  def apply[M, A](
    monoid: FiniteSets.Monoid[M]
  )(
    generatorsWithRelators: List[GeneratorWithRelators[M, A]]
  ) : monoid.Presentation[Int] = {
    val monoidElements: List[M] =
      elementsOf(monoid.carrier).toList
    val lookupMonoid: Map[M, Int] =
      monoidElements.zipWithIndex.toMap
    val generators: List[A] =
      generatorsWithRelators map { _.generator }
    val lookupGenerator: Map[A, Int] =
      generators.zipWithIndex.toMap
    
    val words = makeDot(generators) x monoid.carrier
    
    def indexOfWord(g: A, m: M): Int =
        lookupGenerator(g) * monoidElements.size + lookupMonoid(m)

    def wordOfIndex(index: Int): (A, M) =
      generators(index / monoidElements.size) ->
          monoidElements(index % monoidElements.size)
    
    val equivalenceTable: Seq[Int] =
      BuildEquivalence(
        generators.size * monoidElements.size,
        for {
          gr <- generatorsWithRelators
          g = gr.generator
          Relator(m, index, n) <- gr.relators
          p <- monoidElements
        }
          yield (
            indexOfWord(g, monoid.multiply(m, p)),
            indexOfWord(generators(index), monoid.multiply(n, p))
          )
      )
     
//     println("equivalenceTable = " + equivalenceTable)
    val wordIndices = equivalenceTable.toSet
    val wordIndicesDot = makeDot(wordIndices)
     
    new monoid.Presentation[Int] {
      override val action: monoid.Action[Int] =
        monoid.action(wordIndicesDot) { 
          (index, n) =>
            val (g, m) = wordOfIndex(index)
            val mn = monoid.multiply(m, n)
            val product = equivalenceTable(
              indexOfWord(g, mn)
            )
//            println(s"** ${index}*${n} : index=$index -> ($g, $m) -- n=$n --> ($g, $mn) -> $product")  
            product
        }
      override def project[B](
        otherAction: monoid.Action[B],
        targetElements: List[B]
      ): Int > B =
        wordIndicesDot(otherAction.actionCarrier) { 
          index => 
            val (g, m) = wordOfIndex(index)
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


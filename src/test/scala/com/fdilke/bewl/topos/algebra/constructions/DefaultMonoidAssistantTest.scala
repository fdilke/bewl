package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{ ~, functionAsArrow }
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.language.reflectiveCalls
import scala.language.existentials

class DefaultMonoidAssistantTest extends FreeSpec {
  
  private val (i, x, y) = ('i, 'x, 'y)

  private val monoidOf3 =
    monoidFromTable(
      i, x, y,
      x, x, y,
      y, x, y
    ) // right-dominant on two generators
    
  import monoidOf3.regularAction
  
  private val barDot: FiniteSets.DOT[String] = dot("x", "y")
  
  private val scalarMultiply: (String, Symbol) => String =
      (s, m) => monoidOf3.multiply(Symbol(s), m).name
      
  private val bar = monoidOf3.action(barDot)(scalarMultiply)

  private val analyzer =
    FiniteSets.DefaultMonoidAssistant.actionAnalyzer(
      monoidOf3          
    )

  private val regularAnalysis =
    analyzer.analyze(regularAction)
      
  "The default monoid assistant" - {
    "can enumerate morphisms" in {
      regularAnalysis.morphismsTo(bar).toSet shouldBe {
        elementsOf(barDot).toSet map { (a: String) =>
          regularAction.actionCarrier(barDot) { m => 
            scalarMultiply(a, m)
          }
        }
      }
    }
  }
}
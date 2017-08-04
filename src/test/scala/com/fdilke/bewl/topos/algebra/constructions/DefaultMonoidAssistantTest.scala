package com.fdilke.bewl.topos.algebra.constructions

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.{ ~, functionAsArrow, bifunctionAsBiArrow }
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import com.fdilke.bewl.helper.⊕
import Function.untupled
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
    
//    "can calculate raw exponentials" in {
//      val bazDot = dot("i", "x", "y")
//      val baz = monoidOf3.action(bazDot)(scalarMultiply)
//      val barAnalysis = analyzer.analyze(bar)
//    
//      val rawExponential = barAnalysis.rawExponential(baz)
//      rawExponential.exponentialAction.sanityTest()
//      rawExponential.evaluation should have(
//        'source(rawExponential.exponentialAction.actionCarrier x barDot),
//        'target(bazDot)
//      )
//      monoidOf3.actions.isMorphism(
//        rawExponential.exponentialAction x bar, 
//        targetAlgebra, 
//        rawExponential.evaluation
//      ) shouldBe true
//
//      val foo = regularAction
//      val foobar2baz = bifunctionAsBiArrow(
//          foo.actionCarrier, 
//          barDot, 
//          bazDot
//        )(untupled (Map(
//          (i, "x") -> "x", (x, "x") -> "x", (y, "x") -> "y",
//          (i, "y") -> "y", (x, "y") -> "x", (y, "y") -> "y"
//        )))
//
//      val foo2bar2baz = barAnalysis.transpose(foobar2baz)
//      foo2bar2baz.sanityTest()
//      foo2bar2baz should have(
//        'source(foo),
//        'target(rawExponential.exponentialAction.actionCarrier)
//      )
//      (foo.actionCarrier x barDot)(bazDot) {
//        case f ⊕ b =>
//          rawExponential.evaluation(foo2bar2baz(f), b)
//      } shouldBe foobar2baz.arrow
//    }
  }
}
package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.DiagrammaticFiniteSets.DiagrammaticFiniteSetsDot
import com.fdilke.bewl.fsets.LayeredFiniteSets._

object ExploringLayeredFiniteSets extends App {

  val blank = makeDot(DiagrammaticFiniteSetsDot[Symbol](Seq()))
  val one = makeDot(DiagrammaticFiniteSetsDot(Seq(1)))
  val blank2one = functionAsArrow(blank, one, (_: Symbol) => 1)

  println(s"-2: blank2one.isMonic = ${blank2one.isMonic}")

  val O = TruthObject.falsity.whereTrue

  println(s"-1.5: falsity.isMonic = ${TruthObject.falsity.isMonic}")

  def fromO[X <: ~](dot: DOT[X]) = {

    TruthObject.falsity.sanityTest
    dot.toI.sanityTest
    println("Falsity amd dot.toI are sane")
    println("Falsity = " + TruthObject.falsity)
    println("dot.toI = " + dot.toI)
    println("Calculating carefulCompose...")
    val carefulCompose = TruthObject.falsity o dot.toI
    println("Calculating carefulCompose... done")
    println("carefulCompose = " + carefulCompose)

    println(s"sanity1: ${dot.toI.source == dot}")
    println(s"sanity2: ${dot.toI.target == I}")
    println(s"sanity3: ${I == TruthObject.falsity.source}")
    println(s"sanity4: ${omega == TruthObject.falsity.target}")
    println(s"sanity5: calcalc := ${TruthObject.falsity o dot.toI} =: cancalc")

    val xO = dot.toTrue ?= (TruthObject.falsity o dot.toI)
    xO.sanityTest
    println("Sane")
    println(s"Doing the restriction is sensible:${
      (TruthObject.falsity o xO.toI) == (truth o xO.toI)
    }" )
    println("Calc'ing restriction")
    println("Even more basic sanity test: " + (xO.toI.source == xO))
    val xOtoO = O.restrict(xO.toI)
    println("Calc'd restriction")
    println(s"-1: O.inclusion = " + O.inclusion)
    println(s"0: " + O.inclusion.isMonic)
    xOtoO.sanityTest
    println(s"1: " + (xOtoO.source == xO))
    println(s"2: " + (xOtoO.target == O))
    xO.inclusion o xOtoO.inverse
  }

  fromO(O)
}

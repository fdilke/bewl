package com.fdilke.bewl2.experiments

class TooCleverByHalfRightIdeals:
  // type RI = rightIdeals.RIGHT_IDEAL
  lazy val rightIdeals: RightIdeals =
    nastyCalc()

  private def nastyCalc(): RightIdeals =
    println("I was hoping to not do this calculation")
    new RightIdeals:
      override type RIGHT_IDEAL = Boolean
  
trait RightIdeals:
  type RIGHT_IDEAL

// abstract class NastyCalc extends HasSpecialType  

object TooCleverByHalfRightIdeals extends App:
  println("Instantiating a TCBHRI...")
  println("Instantiating a TCBHRI... done")
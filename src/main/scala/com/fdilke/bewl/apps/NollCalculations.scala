package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{Monoid, ToposOfMonoidActions, bifunctionAsBiArrow, makeDot, x}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{elementsOf, makeNullaryOperator}
import com.fdilke.bewl.helper.⊕

import scala.language.postfixOps

object NollCalculations extends App {

  val octaveLength = 12
  val octaveDot = makeDot(0 until octaveLength)
  val inChord =
    octaveDot(FiniteSets.omega) {
      Seq(0, 4, 7).contains(_)
    }
  val chordDot = inChord.whereTrue

  val affineMapsDot = octaveDot.squared

  private def affineMapMultiply(
    ab: Int x Int,
    cd: Int x Int
  ): (Int x Int) =
    (ab, cd) match {
      case (a ⊕ b, c ⊕ d) =>
        affineMapsDot.pair(
          (a * c) % octaveLength,
          (b * c + d) % octaveLength
        )
    }

  if(false)
  {
    val affineMaps =
      new Monoid[Int x Int](
        affineMapsDot,
        makeNullaryOperator(
          affineMapsDot,
          affineMapsDot.pair(1, 0)
        ),
        bifunctionAsBiArrow(
          affineMapsDot
        )(
          affineMapMultiply
        )
      )

    affineMaps.sanityTest()
  }

  private def affineMapApply(
    a: Int,
    pq: Int x Int
  ): Int =
    pq match {
      case p ⊕ q =>
        (p * a + q) % octaveLength
    }

  val triadicMapsDot =
    affineMapsDot.whereAll(
      chordDot
    ) { (map, note) =>
      inChord(
        affineMapApply(
          note,
          map
        )
      )
    }

  println("# triadic maps = " + elementsOf(triadicMapsDot).size)

  val triadicMonoid =
    new Monoid[Int x Int](
      triadicMapsDot,
      makeNullaryOperator(
        triadicMapsDot,
        affineMapsDot.pair(1, 0)
      ),
      bifunctionAsBiArrow(
        triadicMapsDot
      )(
        affineMapMultiply
      )
    )

  triadicMonoid.sanityTest()

  val octaveAction =
    triadicMonoid.action(
      octaveDot
    ) ( affineMapApply )

  octaveAction.sanityTest()

  val chordAction =
    triadicMonoid.action(
      chordDot
    ) ( affineMapApply )

  chordAction.sanityTest()

  // now construct the topos and some dots in it

  val triadicTopos =
    ToposOfMonoidActions of triadicMonoid

  val octave =
    triadicTopos.makeDot(
      octaveAction
    )
  
  val chord =
    triadicTopos.makeDot(
      chordAction
    )

  val cyclic =
    triadicTopos.makeDot(
      triadicMonoid.regularAction
    )

  def measure[T](
    tune: triadicTopos.DOT[T]
  ) =
    elementsOf(
      triadicTopos.unwrap(
        tune
      ).actionCarrier
    ) size

  def showInjectivity[T](
    name: String,
    tune: triadicTopos.DOT[T]
  ) {
    println(s"$name injective: ")
//    println(tune.isInjective)
    val power = tune.power
    println("calculated power")
    println("size of omega = " + measure(triadicTopos.omega))
    println("size of power = " + measure(power))
    (power >> triadicTopos.I) foreach { to1 =>
      println("calculated power -> 1")
    }
    println("calculated power -> 1... done")
    val isInjective =
      (power >> tune) exists { projection =>
        print("*")
        val retracts =
          (projection o tune.singleton) == tune.identity
        if (retracts)
          println("!")
        retracts
      }
    println(isInjective)
  }

  showInjectivity("chord", chord)
  showInjectivity("cyclic", cyclic)
  showInjectivity("octave", octave)
}

package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}
import com.fdilke.bewl.fsets.FiniteSets.{Monoid, bifunctionAsBiArrow, makeDot, x}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{elementsOf, makeNullaryOperator}
import com.fdilke.bewl.helper.⊕

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

  val triadicMaps =
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

  triadicMaps.sanityTest()

  val octave =
    triadicMaps.action(
      octaveDot
    ) ( affineMapApply )

  octave.sanityTest()

  val notSane =
    triadicMaps.action(
      chordDot
    ) ( affineMapApply )

  notSane.sanityTest()

}

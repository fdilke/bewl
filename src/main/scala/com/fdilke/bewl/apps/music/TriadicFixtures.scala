package com.fdilke.bewl.apps.music

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{
  bifunctionAsBiArrow,
  makeDot,
  x,
  EQUALIZER,
  FiniteSetsDot,
  Monoid,
  ToposOfMonoidActions
}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.makeNullaryOperator
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.{Topos, Wrappings}
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

object TriadicFixtures {

  val octaveLength = 12
  val octaveDot: FiniteSetsDot[Int] =
    makeDot(0 until octaveLength)

  private val inChord: FiniteSets.>[Int, Boolean] =
    octaveDot(FiniteSets.omega) {
      Seq(0, 1, 4).contains(_)
    }

  val chordDot: EQUALIZER[Int] =
    inChord.whereTrue

  val affineMapsDot = octaveDot.squared

  def affineMapApply(
    a: Int,
    pq: Int x Int
  ): Int =
    pq match {
      case p ⊕ q =>
        (p * a + q) % octaveLength
    }

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

  // affineMaps.sanityTest Takes too long :0

  private val triadicMapsDot: EQUALIZER[FiniteSets.x[Int, Int]] =
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

  triadicMapsDot should (have size 8)

  def affineMapMultiply(
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

  triadicMonoid.sanityTest

  val octaveAction =
    triadicMonoid.action(
      octaveDot
    )(affineMapApply)

  octaveAction.sanityTest

  val chordAction =
    triadicMonoid.action(
      chordDot
    )(affineMapApply)

  chordAction.sanityTest

  // now construct the topos and some dots in it

  val triadicTopos: Topos[Any]
    with Wrappings[
      Any,
      Any,
      ({ type λ[X] = triadicMonoid.Action[X] })#λ,
      ({ type λ[X, Y] = triadicMonoid.ActionPreArrow[X, Y] })#λ,
      ({ type λ[T] = T })#λ
    ] =
    ToposOfMonoidActions.of(triadicMonoid)

  val octave: triadicTopos.DOT[Int] =
    triadicTopos.makeDot(
      octaveAction
    )

  val chord: triadicTopos.DOT[Int] =
    triadicTopos.makeDot(
      chordAction
    )

  val cyclic: triadicTopos.DOT[FiniteSets.x[Int, Int]] =
    triadicTopos.makeDot(
      triadicMonoid.regularAction
    )
}

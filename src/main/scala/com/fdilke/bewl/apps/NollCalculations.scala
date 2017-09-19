package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{ActionSplitter, Monoid, Relation, ToposOfMonoidActions, bifunctionAsBiArrow, makeDot, x}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{elementsOf, makeNullaryOperator}
import com.fdilke.bewl.helper.{Timed, ⊕}

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

  def triadicSplit[A](
    dot: triadicTopos.DOT[A]
  ) =
    ActionSplitter.forMonoid(
      triadicMonoid
    ).splitAction(
      triadicTopos.unwrap(
        dot
      )
    )

  if(true)  {
    val cc = cyclic x chord
    val ccAnalysis =
      triadicSplit(cc)
    println(
      "\tcyclic x chord: number of components: " + (
        ccAnalysis.components.size
      )
    )
    val pc = Timed("calculating power chord") {
      chord.power
    }
    println("power chord size: " +
      (elementsOf(triadicTopos.unwrap(pc).actionCarrier).size)
    )

    val pcAnalysis =
      Timed("calculating components of power chord") {
        triadicSplit(pc)
      }
    println(
      "\tpower chord: number of components: " + (
        pcAnalysis.components.size
      )
    )
  }

  if (false) { // too slow, as yet
    println("Chord square:")
    val c2 = chord x chord
    val csAnalysis =
      ActionSplitter.forMonoid(
        triadicMonoid
      ).splitAction(
        chordAction x chordAction
      )
    println(
      "\tnumber of components: " + (
        csAnalysis.components.size
      )
    )
    println(
      "\ttotal generators: " + (
        csAnalysis.allGenerators.size
      )
    )
    print("c2 >> omega: ")
    val c2_omega = c2 >> triadicTopos.omega
    for {
      arrow <- c2_omega
    } {
      print("*")

      val relation =
        triadicTopos.Relation(
          chord,
          chord,
          triadicTopos.BiArrow(
            c2,
            arrow
          )
        )

      import triadicTopos.EndoRelation

      print("[")
      print(if (relation.isReflexive) "R" else "r")
      print(if (relation.isSymmetric) "S" else "s")
      print(if (relation.isIdempotent) "I" else "i")
      print("]")
    }
    println(" (" + (c2_omega.size) + ")")
  }

  def measure[T](
    tune: triadicTopos.DOT[T]
  ) =
    elementsOf(
      triadicTopos.unwrap(
        tune
      ).actionCarrier
    ) size

  def showProperties[T](
    name: String,
    tune: triadicTopos.DOT[T]
  ) {
    val tuneMinimal =
      Timed(s"calc $name minimal") {
        tune.isMinimal
      }
    println(s"$name minimal: " + tuneMinimal)
    val tuneSimple =
      Timed(s"calc $name minimal") {
        tune.isSimple
      }
    println(s"$name minimal: " + tuneSimple)

    if (false) {
      val tuneInjective =
        Timed(s"calc $name injective") {
          tune.isInjective
        }
      println(s"$name minimal: " + tuneInjective)

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
  }

  showProperties("chord", chord)
  showProperties("cyclic", cyclic)
  showProperties("octave", octave)
}

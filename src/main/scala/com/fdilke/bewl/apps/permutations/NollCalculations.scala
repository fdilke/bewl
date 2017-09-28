package com.fdilke.bewl.apps.permutations

import com.fdilke.bewl.fsets.FiniteSets
import FiniteSets.ActionSplitter
import com.fdilke.bewl.fsets.FiniteSetsUtilities.elementsOf
import com.fdilke.bewl.helper.Timed
import TriadicFixtures.{triadicTopos, _}
import triadicTopos.EndoRelation

import scala.language.postfixOps

object NollCalculations extends App {

  def triadicSplit[A](
    dot: triadicTopos.DOT[A]
  ): FiniteSets.ActionSplitting[
    FiniteSets.x[Int, Int],
    A,
    ({ type λ[T] =
      triadicMonoid.Action[T] })#λ
  ] =
    ActionSplitter.forMonoid(
      triadicMonoid
    ).splitAction(
      triadicTopos.unwrap(
        dot
      )
    )

  if (true) {
    for {
      n <- elementsOf(chordDot)
    } {
      val generated =
        elementsOf(triadicMonoid.carrier).map {
          a => affineMapApply(n, a)
        }.toSeq.distinct.sorted

      println(n + " -> " + generated.mkString(","))
    }
    println("#chord subobjects = " +
      (chord >> triadicTopos.omega).size)

    for {
      chi <- chord >> triadicTopos.omega
    } {
      val sub = chi.whereTrue
      println("sub> " + elementsOf(
        triadicTopos.unwrap(sub).actionCarrier
      ).toSeq.sorted.mkString(","))
    }
    System.exit(0)
  }

  if(true)  {
    val cc = cyclic x cyclic
    val ccAnalysis =
      triadicSplit(cc)
    println(
      "\tcyclic x cyclic: number of components: " + (
        ccAnalysis.components.size
      )
    )
    val nub = Timed("calculating the nub") {
      cyclic > triadicTopos.omega
    }
    println("nub size: " +
      triadicTopos.unwrap(nub).actionCarrier.size
    )

    val nubAnalysis =
      Timed("calculating components of the nub") {
        triadicSplit(nub)
      }
    println(
      "\tnub: number of components: " + (
        nubAnalysis.components.size
      )
    )
  }

  if(false)  {
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
      triadicTopos.unwrap(pc).actionCarrier.size
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

  if (true) { // too slow, as yet
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
    triadicTopos.unwrap(
      tune
    ).actionCarrier size

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
      Timed(s"calc $name simple") {
        tune.isSimple
      }
    println(s"$name simple: " + tuneSimple)

    val tuneInjective =
      Timed(s"calc $name injective") {
        tune.isInjective
      }
    println(s"$name injective: " + tuneInjective)

    if (false) {
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

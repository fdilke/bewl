package com.fdilke.bewl.apps.music

import com.fdilke.bewl.apps.TwitterHack
import com.fdilke.bewl.apps.TwitterHack.tweetMe
import com.fdilke.bewl.apps.music.TriadicFixtures.{triadicTopos, _}
import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.ActionSplitter
import triadicTopos.EndoRelation
import com.fdilke.bewl.fsets.FiniteSetsUtilities.elementsOf
import com.fdilke.bewl.helper.{⊕, IterateToFixed, Timed}

import scala.language.postfixOps

object NollCalculations extends App {

  def triadicSplit[A](
    dot: triadicTopos.DOT[A]
  ): FiniteSets.ActionSplitting[
    FiniteSets.x[Int, Int],
    A,
    ({
      type λ[T] =
        triadicMonoid.Action[T]
    })#λ
  ] =
    ActionSplitter
      .forMonoid(
        triadicMonoid
      )
      .splitAction(
        triadicTopos.unwrap(
          dot
        )
      )

  if (false) {
    val subobjs: Iterable[triadicTopos.>[Int, triadicTopos.TRUTH]] =
      Timed("calculating subobjects of the octave") {
        octave >> triadicTopos.omega
      }
    println("#subobjs: " + subobjs.size)

    def subSize[A](sub: triadicTopos.EQUALIZER[A]) =
      elementsOf(triadicTopos.unwrap(sub).actionCarrier).size

    println("Number of each size:")

    val sizeGroups: Map[Int, Iterable[triadicTopos.EQUALIZER[Int]]] =
      subobjs.map(_.whereTrue).groupBy(subSize)

    val sortedSizes: Seq[Int] =
      sizeGroups.keySet.toSeq.sorted

    for { size <- sortedSizes } println("size " + size + " : " + sizeGroups(size).size)

    for {
      size <- sortedSizes
      sizeGroup = sizeGroups(size)
      sub <- sizeGroup
      subelts = elementsOf(triadicTopos.unwrap(sub).actionCarrier)
    } {
      val name = "[" + subelts.mkString(",") + "]"

//      println("! " + name)
      tweetMe("calc injectivity of " + name)

      val isInjective = Timed("calc injectivity of " + name) {
        sub.isInjective
      }
      if (isInjective) {
        tweetMe("INJECTIVE!" + name)
        println("INJECTIVE")
      }
    }
    println("subobjs: done")
    tweetMe("subobjs: done")
    System.exit(0)
  }

  if (false) {
    for {
      n <- elementsOf(octaveDot)
    } {
      val generated =
        elementsOf(triadicMonoid.carrier).map(a => affineMapApply(n, a)).toSeq.distinct.sorted

      println(n + " -> " + generated.mkString(","))
    }
    System.exit(0)
  }

  if (false) {
    for {
      n <- elementsOf(chordDot)
    } {
      val generated =
        elementsOf(triadicMonoid.carrier).map(a => affineMapApply(n, a)).toSeq.distinct.sorted

      println(n + " -> " + generated.mkString(","))
    }
    println(
      "#chord subobjects = " +
        (chord >> triadicTopos.omega).size
    )

    for {
      chi <- chord >> triadicTopos.omega
    } {
      val sub = chi.whereTrue
      println(
        "sub> " + elementsOf(
          triadicTopos.unwrap(sub).actionCarrier
        ).toSeq.sorted.mkString(",")
      )
    }
    System.exit(0)
  }

  if (false) {
    val cc = cyclic.x(cyclic)
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
    println(
      "nub size: " +
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

  if (false) {
    val cc = cyclic.x(chord)
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
    println(
      "power chord size: " +
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

  if (true) { // fast enough now
    println("Chord square:")
    val c2 = chord.x(chord)
    val csAnalysis =
      ActionSplitter
        .forMonoid(
          triadicMonoid
        )
        .splitAction(
          chordAction.x(chordAction)
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
      println("]")
      if (relation.isEquivalence) {
        import ⊕._
//        val relationObject =
//          relation.criterion.arrow.whereTrue
        val congruence = // Int x Int > TRUTH
          relation.criterion.arrow
        val nameTruth =
          triadicMonoid.carrier.toTrue.name(())
        println("global name of truth = " + nameTruth)
        val pairs =
          for {
            i <- elementsOf(chordDot)
            j <- elementsOf(chordDot) if {
              i < j && (
                congruence(i ⊕ j) == nameTruth
              )
            }
          } yield {
            i -> j
          }
        println("criterion = " + pairs)
      }
    }
    println(" (" + (c2_omega.size) + ")")
    System.exit(0)
  }

  def measure[T](
    tune: triadicTopos.DOT[T]
  ) =
    triadicTopos
      .unwrap(
        tune
      )
      .actionCarrier size

  def showProperties[T](
    name: String,
    tune: triadicTopos.DOT[T]
  ): Unit = {
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

    if (true) {
      println("calculating power of " + name)
      val power = tune.power
      println("size of omega = " + triadicTopos.omega.size)
      println("size of power = " + measure(power))
      if (false) {
        (power >> triadicTopos.I).foreach(to1 => println("calculated power -> 1"))
        println("calculated power -> 1... done")
        val isInjective =
          Timed("calculating injectivity of " + name) {
            (power >> tune).exists { projection =>
              print("*")
              val retracts =
                (projection.o(tune.singleton)) == tune.identity
              if (retracts)
                println("!")
              retracts
            }
          }
        println(isInjective)
      }

      val tuneInjective =
        Timed(s"calc $name injective") {
          tune.isInjective
        }
      println(s"$name injective: " + tuneInjective)
    }
  }

  showProperties("chord", chord)
  showProperties("cyclic", cyclic)
  showProperties("octave", octave)
}

object LogicalOperationsSlow extends App {
  if (false) {
    val omegaElements: Iterable[triadicTopos.TRUTH] =
      elementsOf(
        triadicTopos
          .unwrap(
            triadicTopos.omega
          )
          .actionCarrier
      )

    import triadicTopos.OmegaEnrichments

    for {
      ω <- omegaElements
      ξ <- omegaElements
    } {
      println("Next ∧ calculation:")
      println("ω, ξ = " + ω + "," + ξ)
      println("ω ∧ ξ = " + (ω ∧ ξ))
    }
  }
  if (false) {
    val delazify = triadicTopos
    println("computing Ω...")
    val o = triadicTopos.Ω
    println("computing Ω... done")
  }
  if (true) {
    val oo = triadicTopos.omega
    for {
      arrow <- oo.squared >> oo
    } {
      println("\t" + arrow)
    }
  }

  if (false) {
    def triadicSplit[A](
      dot: triadicTopos.DOT[A]
    ): FiniteSets.ActionSplitting[
      FiniteSets.x[Int, Int],
      A,
      ({
        type λ[T] =
          triadicMonoid.Action[T]
      })#λ
    ] =
      ActionSplitter
        .forMonoid(
          triadicMonoid
        )
        .splitAction(
          triadicTopos.unwrap(
            dot
          )
        )

    val omegaSplit =
      triadicSplit(
        triadicTopos.omega.squared // x triadicTopos.optionalGenerator.get
      )
    val generators =
      omegaSplit.allGenerators
    println("Components(" + omegaSplit.components.size + ")")
    println("Generators (" + generators.size + "):")
    println(generators)
  }
}

object ChordRelationsSlow extends App {
  val diag =
    triadicTopos.Relation.diagonalRelation(chord)

  val ans =
    Timed("Composing the diagonal") {
      diag.o(diag)
    }
}

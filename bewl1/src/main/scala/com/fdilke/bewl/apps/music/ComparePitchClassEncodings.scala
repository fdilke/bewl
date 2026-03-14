package com.fdilke.bewl.apps.music

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{bifunctionAsBiArrow, x, EQUALIZER, Monoid}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{elementsOf, makeNullaryOperator}
import TriadicFixtures._
import com.fdilke.bewl.helper.{⊕, IterateToFixed}
import ⊕._

import scala.language.postfixOps

object ComparePitchClassEncodings extends App {

// TODO fix: too slow!
//  val affineGroup =
//    groupOfUnits(
//      affineMaps
//    )._1

  println("PCE 1")

  val informalGroup: EQUALIZER[FiniteSets.x[Int, Int]] =
    affineMapsDot(FiniteSets.omega) { m =>
      elementsOf(octaveDot)
        .map {
          affineMapApply(_, m)
        }
        .toSet
        .size == octaveLength
    } whereTrue

  println("PCE 2")

  assert(informalGroup.size == 48)

  val circle5Encoding = Seq(0, 1, 4)
  val semitoneEncoding = Seq(0, 4, 7)

  val isomorphicTriads =
    elementsOf(informalGroup)
      .map { g =>
        circle5Encoding.map {
          affineMapApply(_, g)
        }.sorted
      }
      .toSeq
      .distinct
      .filter {
        _.head == 0
      }

  isomorphicTriads.foreach(triad => println(triad.mkString(",")))

  assert(
    isomorphicTriads.contains(
      semitoneEncoding
    )
  )
}

object StabilizerSanity extends App {
  // verify that:
  // stab(C), stab(E) < stab(E)
  // where the stabilizer of an element of an action is:
  // stab(a) = the congruence identifying p, q whenever ap = aq
  // i.e. kernel of left multiplication by a, an algebra morphism M -> aM

//  println("StabilizerSanity")
//  affineMapsDot
//  println("StabilizerSanity 2")
//  affineMapsDot.squared
//  println("StabilizerSanity 3")

  def stab(a: Int) =
    elementsOf(affineMapsDot.squared).filter {
      case p ⊕ q =>
        affineMapApply(a, p) == affineMapApply(a, q)
    } toSet

  val (c, e, g) = (0, 4, 1)
  println("stab(c) size =" + stab(c).size)
  println("stab(e) size =" + stab(e).size)
  println("stab(g) size =" + stab(g).size)
  println
  println("c == e ? " + (stab(c) == stab(e)))
  println("c == g ? " + (stab(c) == stab(g)))
  println("e == g ? " + (stab(e) == stab(g)))
  println("e == e ? " + (stab(e) == stab(e)))
}

object TriadicGenerators extends App {
  for {
    i <- elementsOf(triadicMonoid.carrier)
    j <- elementsOf(triadicMonoid.carrier)
  } {
    val submonoid =
      IterateToFixed(
        Set(triadicMonoid.unit(()), i, j)
      ) { set =>
        set.union(
          for { i <- set; j <- set } yield {
            triadicMonoid.multiply(i, j)
          }
        )
      }
    val size = submonoid.size
    if (size == triadicMonoid.carrier.size)
      println(s"M = <$i, $j>")
  }
}

object GeneratorsOnChord extends App {
  val p = 3 ⊕ 1
  val q = 8 ⊕ 4

  for {
    a <- elementsOf(chordDot)
  } {
    val ap = affineMapApply(a, p)
    val aq = affineMapApply(a, q)
    println(s"$a -p-> ${ap} ; -q-> ${aq}")
  }
}

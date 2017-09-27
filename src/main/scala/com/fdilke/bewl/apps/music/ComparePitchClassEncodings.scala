package com.fdilke.bewl.apps.music

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{EQUALIZER, Monoid, bifunctionAsBiArrow, x}
import com.fdilke.bewl.fsets.FiniteSetsUtilities.{elementsOf, makeNullaryOperator}
import com.fdilke.bewl.apps.permutations.TriadicFixtures._
import org.scalatest.Matchers._

import scala.language.postfixOps

object ComparePitchClassEncodings extends App {

  println("PCE 1")
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
  println("PCE 2")

  affineMaps.sanityTest()

  println("PCE 3")

// TODO fix: too slow!
//  val affineGroup =
//    groupOfUnits(
//      affineMaps
//    )._1

  println("PCE 4")

  val informalGroup: EQUALIZER[FiniteSets.x[Int, Int]] =
    affineMapsDot(FiniteSets.omega) { m =>
      elementsOf(octaveDot).map {
        affineMapApply(_, m)
      }.toSet.size == octaveLength
    } whereTrue

  println("PCE 4")

  elementsOf(informalGroup) should have size 48

  val circle5Encoding = Seq(0, 1, 4)
  val semitoneEncoding = Seq(0, 4, 7)

  val isomorphicTriads =
    elementsOf(informalGroup).map { g =>
      circle5Encoding.map {
        affineMapApply(_, g)
      }.sorted
    }.toSeq.distinct filter {
      _.head == 0
    }

  isomorphicTriads foreach { triad =>
    println(triad mkString ",")
  }

  isomorphicTriads should contain(
    semitoneEncoding
  )
}

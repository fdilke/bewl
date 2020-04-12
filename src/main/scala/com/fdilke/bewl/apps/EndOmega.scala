package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities.elementsOf
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import scala.language.existentials

object EndOmega extends App {
  val topos =
    FiniteSets.ToposOfMonoidActions.of(monoidOf3)

  import topos.{>, omega, unwrap}

  doIt(
    elementsOf(
      unwrap(
        omega
      ).actionCarrier
    ),
    omega >> omega
  )

  // trick to get round 'cyclic dependency' issue: abstract the type

  private def doIt[IDEAL](
    ideals: Iterable[IDEAL],
    mappings: Iterable[IDEAL > IDEAL]
  ): Unit = {
    assert(ideals.size == 3)
    val Seq(_m, _i, _o) = ideals

    val idealToName: Map[IDEAL, String] =
      Map(
        _m -> "M",
        _i -> "I",
        _o -> "O"
      )

    ideals.foreach(ii => println(idealToName(ii) + " = " + ii))
    println

    for {
      f <- mappings
    } println(
      ideals.map(ii => idealToName(ii) + " => " + idealToName(f(ii))).mkString(", ")
    )
  }
}

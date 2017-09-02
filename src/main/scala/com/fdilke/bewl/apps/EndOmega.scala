package com.fdilke.bewl.apps

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSetsUtilities.elementsOf
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3

object EndOmega extends App {
  val topos =
    FiniteSets.ToposOfMonoidActions.of(monoidOf3)

  import topos.{ unwrap, omega, > }

  doIt(
    elementsOf(
      unwrap(
        omega
      ).actionCarrier
    ),
    omega >> omega
  )

  // trick to get round 'cyclic dependency' issue: abstract the type

  def doIt[IDEAL](
    ideals: Traversable[IDEAL],
    mappings: Traversable[IDEAL > IDEAL]
  ) {
    assert( ideals.size == 3 )
    val Seq(_m, _i, _o) = ideals

//    "M" -> Map('y -> true, 'x -> true, 'i -> true),
//    "I" -> Map('y -> true, 'x -> true, 'i -> false),
//    "O"  -> Map('y -> false, 'x -> false, 'i -> false)

    val idealToName: Map[IDEAL, String] =
      Map(
        _m -> "M",
        _i -> "I",
        _o -> "O"
      )

    ideals foreach { ii =>
      println(idealToName(ii) + " = " + ii)
    }
    println

    for {
      f <- mappings
    }
      println(
        ideals map { ii =>
          idealToName(ii) + " => " + idealToName(f(ii))
        } mkString ", "
      )
  }
}

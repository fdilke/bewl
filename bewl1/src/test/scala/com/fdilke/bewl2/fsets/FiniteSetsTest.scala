package com.fdilke.bewl2.fsets

import com.fdilke.bewl.fsets.FiniteSetsUtilities.arrow
import com.fdilke.bewl2.fsets.FiniteSets._
import com.fdilke.bewl2.topos.GenericToposTests

import scala.Function.untupled

private object Fixtures {
  implicit val foo: Iterable[Boolean] = Iterable(true, false)
  implicit val bar: Iterable[String] = Iterable("X", "Y", "Z")
  implicit val baz: Iterable[Int] = Iterable(1, 2, 3, 4)
}

import Fixtures._

class FiniteSetsTest extends GenericToposTests[Iterable, Boolean, String, Int] {
  override val foo2bar = Map(
    true -> "X",
    false -> "Y"
  )
  override val foo2ImageOfBar = Map(
    true -> 3,
    false -> 2
  )
  override val monicBar2baz = Map(
    "X" -> 2,
    "Y" -> 3,
    "Z" -> 1
  )

  override val foobar2baz = untupled(
    Map(
      (true, "X") -> 2,
      (false, "X") -> 3,
      (true, "Y") -> 1,
      (false, "Y") -> 2,
      (true, "Z") -> 2,
      (false, "Z") -> 3
    )
  )

  override def provideEqualizerSituation[X](
    receiver: EqualizerSituationReceiver[X]
  ): X =
    receiver(
      new EqualizerSituation[Boolean, String, Int](
        foo2bar,
        Map("X" -> 1, "Y" -> 2, "Z" -> 3),
        Map("X" -> 1, "Y" -> 2, "Z" -> 1)
      )
    )
}

package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposTests
import munit.FunSuite

class SetsSpec extends GenericToposTests()(Sets):

  import topos.*

  override type FOO = Boolean
  override implicit val dotFoo: Set[Boolean] =
    Set(true, false)

  override type BAR = String
  override implicit val dotBar: Set[String] =
    Set("one", "two", "three")

  override type BAZ = Int
  override implicit val dotBaz: Set[Int] =
    Set(1, 2, 3, 4)

  override val foo2bar: FOO ~> BAR =
    arrow(Map[Boolean, String](elems =
        true -> "one",
        false -> "two"
      ))

  override val foo2baz: Boolean ~> Int =
    arrow(Map[Boolean, Int](elems =
        true -> 3,
        false -> 2
      ))

  override val foobar2baz: (Boolean, String) ~> Int =
    arrow(Map[(Boolean, String), Int](elems =
        (true, "one") -> 2,
        (false, "one") -> 3,
        (true, "two") -> 1,
        (false, "two") -> 2,
        (true, "three") -> 2,
        (false, "three") -> 3
      ))

  override val equalizerSituation: EqualizerSituation[_, _, _] =
    new EqualizerSituation[FOO, BAR, BAZ](
      foo2bar,
      arrow(Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3)),
      arrow(Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 1))
    )


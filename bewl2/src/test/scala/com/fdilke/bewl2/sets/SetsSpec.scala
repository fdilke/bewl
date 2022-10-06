package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposTests
import munit.FunSuite

class SetsSpec extends GenericToposTests()(Sets):

  import topos.*

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction

  override type FOO = Direction
  override implicit val dotFoo: Set[Direction] =
    Set(Up, Down)

  override type BAR = String
  override implicit val dotBar: Set[String] =
    Set("one", "two", "three")

  override type BAZ = Int
  override implicit val dotBaz: Set[Int] =
    Set(1, 2, 3, 4)

  override val foo2bar: FOO ~> BAR =
    arrow(Map[Direction, String](elems =
        Up -> "one",
        Down -> "two"
      ))

  override val foo2baz: Direction ~> Int =
    arrow(Map[Direction, Int](elems =
        Up -> 3,
        Down -> 2
      ))

  override val foobar2baz: (Direction, String) ~> Int =
    arrow(Map[(Direction, String), Int](elems =
        (Up, "one") -> 2,
        (Down, "one") -> 3,
        (Up, "two") -> 1,
        (Down, "two") -> 2,
        (Up, "three") -> 2,
        (Down, "three") -> 3
      ))

  override val equalizerSituation: EqualizerSituation[_, _, _] =
    new EqualizerSituation[FOO, BAR, BAZ](
      foo2bar,
      arrow(Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3)),
      arrow(Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 1))
    )


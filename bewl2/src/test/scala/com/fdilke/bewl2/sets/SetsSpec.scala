package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposSpec
import munit.FunSuite
import Piggis.Direction
import Direction._

class SetsSpec extends GenericToposSpec()(Sets):
  import topos.*

  override type FOO = Direction
  override type BAR = String
  override type BAZ = Int
  
  override def withTestDots(
    block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit =
    withDots(
      Set[Direction](Up, Down),
      Set[String]("one", "two", "three"),
      Set[Int](1, 2, 3, 4)
    ) {
      block(
        new ToposFixtures {
          override val foo2bar: Direction ~> String =
            Map[Direction, String](elems =
              Up -> "one",
              Down -> "two"
            )

          override val foo2baz: Direction ~> Int =
            Map[Direction, Int](elems =
              Up -> 3,
              Down -> 2
            )

          override val monicBar2baz: String ~> Int =
            Map[String, Int](elems =
              "one" -> 2,
              "two" -> 3,
              "three" -> 1
            )

          override val foobar2baz: (Direction, String) ~> Int =
            Map[(Direction, String), Int](elems =
              (Up, "one") -> 2,
              (Down, "one") -> 3,
              (Up, "two") -> 1,
              (Down, "two") -> 2,
              (Up, "three") -> 2,
              (Down, "three") -> 3
            )

          override val foo2ImageOfBar: Direction ~> Int =
            Map[Direction, Int](elems =
              Up -> 3,
              Down -> 2
            )

          override val equalizerSituation: EqualizerSituation[_, _, _] =
            new EqualizerSituation[FOO, BAR, BAZ](
              foo2bar,
              Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 3),
              Map[String, Int]("one" -> 1, "two" -> 2, "three" -> 1)
            )
        }
      )
    }




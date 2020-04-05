package com.fdilke.bewl2.cantorians


import com.fdilke.bewl2.cantorians.Cantorian.cycle
import com.fdilke.bewl2.topology.StrontiumDogEnumeration.StrontiumDog
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl2.topology.StrontiumDogEnumeration._
import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.topology.StrontiumDogEnumeration

class CompactVanillaPitcherTest extends AnyFunSpec {
  describe("Vanilla pitchers of compact things... (working towards Cantorians)") {
    ignore("are implicitly compact: can find solution pitechers where they exist") {
      val solvablePredicates: Seq[(VanillaPitcher[Int => StrontiumDog]) => Boolean] = Seq(
        _.head(0) == Johnny,
        p => p.head(0) != p.tail.head(1),
        p => p.tail.tail.head(0) == Wulf
      )

      solvablePredicates.foreach { predicate =>
        determine[
          VanillaPitcher[Int => StrontiumDog]
        ](predicate) match {
          case None => fail("No solution found")
          case Some(dogPitcher) =>
            predicate(dogPitcher) shouldBe true
        }
      }
    }

    ignore("are implicitly compact: can identify unsolvable pitchers") {
      val unsolvablePredicates: Seq[(VanillaPitcher[Int => StrontiumDog]) => Boolean] = Seq(
        _.head(0).id >= StrontiumDogEnumeration.values.size,
        p => p.head(0).toString == "Nelson Bunker Kreelman",
        p => Set(
          p.head(0),
          p.tail.head(0),
          p.tail.tail.head(1),
          p.tail.tail.head(2)
        ).size == 4
      )

      unsolvablePredicates.foreach { predicate =>
        determine[
          VanillaPitcher[Int => StrontiumDog]
        ](predicate) shouldBe None
      }
    }
  }
}

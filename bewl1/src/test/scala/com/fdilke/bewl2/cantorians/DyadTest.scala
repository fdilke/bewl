package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Dyad.{η, μ, canonical, isPowerOf2}
import com.fdilke.bewl2.cantorians.JonssonTarski.{join, left, right}
import com.fdilke.bewl2.topology.Hausdorff._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

class DyadTest extends AnyFunSpec {
  describe("Dyad helper functions") {
    it("can tell if a number is a power of 2") {
      Seq(
        0, 1, 2, 3, 4, 5, 8, 17, 32
      ).map(isPowerOf2) shouldBe Seq(
        false, true, true, false, true, false, true, false, true
      )
    }
    it("can get a dyadic sequence into canonical form") {
      canonical("x") shouldBe (Seq("x"), 1)
      canonical("A", "A") shouldBe (Seq("A"), 1)
      canonical(true, true) shouldBe (Seq(true), 1)
      canonical(1.0, 2.0) shouldBe (Seq(1.0, 2.0), 2)
      canonical(1.0, 2.0, 1.0, 2.0) shouldBe (Seq(1.0, 2.0), 2)
      canonical(1.0, 2.0, 3.0, 4.0) shouldBe (Seq(1.0, 2.0, 3.0, 4.0), 4)
      canonical(4.0, 4.0, 4.0, 4.0) shouldBe (Seq(4.0), 1)
      canonical(10, 15, 10, 15, 10, 15, 10, 15) shouldBe (Seq(10, 15), 2)
      canonical(6, 5, 0, 2, 6, 5, 0, 2) shouldBe (Seq(6, 5, 0, 2), 4)
      canonical[Boolean => Int](
        if (_) 4 else 4,
        b => (if (b) 8 else 9) / 2
      )._2 shouldBe 1
    }
  }
  describe("Dyads") {
    it("can't be instantiated using 'new'") {
      """new Dyad[String](Seq("X", "Y"), 2)""" shouldNot compile
    }
    it("can be instantiated using a companion factory method") {
      Dyad(2).getClass shouldBe classOf[Dyad[_]]
    }
    it("can be instantiated only with sequences of length a power of 2") {
      Dyad(1)
      Dyad(4, 4, 4, 4)
      Dyad(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)
      intercept[IllegalArgumentException] {
        Dyad(7, 7, 7, 7, 7, 7, 7)
      }
    }
    it("have sane equality semantics") {
      Dyad("A", "B") shouldBe Dyad("A", "B")
      Dyad("A", "B") shouldNot be(Dyad("A"))
      Dyad("B") shouldNot be(Dyad("A"))
      Dyad("A", "B") shouldNot be(Dyad("B", "A"))
    }
    it("have a presentable toString method") {
      Dyad("A", "B").toString shouldBe "Dyad(A,B)"
    }
    it("are instantiated using canonical form") {
      Dyad("A", "A") shouldBe Dyad("A")
      Dyad("A", "A", "A", "A") shouldBe Dyad("A")
      Dyad("A", "B", "C", "D", "A", "B", "C", "D") shouldBe Dyad("A", "B", "C", "D")
      Dyad("A", "B", "A", "B", "A", "B", "A", "B") shouldBe Dyad("A", "B")
    }
    it("support map which coalesces the result into canonical form") {
      Dyad("foo", "barbaz").map(_.length) shouldBe Dyad(3, 6)
      Dyad(1, 2, 3, 4).map(_ % 2) shouldBe Dyad(1, 0)
      Dyad[Boolean => Int](
        if (_) 4 else 4,
        b => (if (b) 8 else 9) / 2
      ) shouldBe Dyad[Boolean => Int](
        if (_) 4 else 4,
        b => (if (b) 8 else 9) / 2
      )
    }
    it("have a length") {
      Dyad(1).length shouldBe 1
      Dyad(1, 7).length shouldBe 2
      Dyad(1, 7, 1, 7).length shouldBe 2
      Dyad(1, 7, 7, 1).length shouldBe 4
    }
    it("can be treated as (doubly infinite) sequences") {
      val dyad: Dyad[String] =
        Dyad("along", "came", "a", "spider")

      dyad(0) shouldBe "along"
      dyad(4) shouldBe "along"
      dyad(2) shouldBe "a"
      dyad(-3) shouldBe "came"
      dyad(287) shouldBe "spider"
      dyad(-666) shouldBe "a"
    }
    it("have a η and μ obeying the monad identity laws") {
      val dyad: Dyad[Int] =
        Dyad(1, 2, 3, 4)

      μ[Int](
        η[Dyad[Int]](dyad)
      ) shouldBe dyad
      μ[Int](
        dyad.map(η[Int])
      ) shouldBe dyad
    }
    it("also μ obeys the monad associativity law") {
      val dyad: Dyad[Dyad[Dyad[Int]]] =
        Dyad(
          Dyad(
            Dyad(7, 2, 3, 4),
            Dyad(1, 8)
          ),
          Dyad(
            Dyad(5, 3, 2, 1)
          )
        )

      μ[Int](
        dyad.map(μ[Int])
      ) shouldBe
        μ[Int](
          μ[Dyad[Int]](dyad)
        )
    }
    it("can be used in for-comprehensions to exploit the monad operations") {
      (for {
        x <- Dyad("one", "at", "too", "on")
      } yield {
        x.length
      }) shouldBe
        Dyad(3, 2)

      (for {
        x <- Dyad("post", "captain")
        y <- Dyad("by", "Patrick", "O", "Brian")
      } yield {
        x + " " + y
      }) shouldBe
        Dyad(
          "post by",
          "captain Patrick",
          "post O",
          "captain Brian"
        )
    }
    it("have a Jonsson-Tarski structure that obeys the axioms") {
      left(Dyad(1)) shouldBe Dyad(1)
      right(Dyad(1)) shouldBe Dyad(1)

      val dyad: Dyad[Int] =
        Dyad(1, 2, 3, 4)
      val dyad2 = dyad.map {
        _ + 10
      }

      left(dyad) shouldBe Dyad(1, 3)
      right(dyad) shouldBe Dyad(2, 4)

      join(dyad, dyad2) shouldBe Dyad(
        1, 11, 2, 12, 3, 13, 4, 14
      )

      join(left(dyad), right(dyad)) shouldBe dyad

      left(join(dyad, dyad2)) shouldBe dyad
      right(join(dyad2, dyad)) shouldBe dyad
    }
    it("inherit Hausdorffness from their type parameter") {
      equalH(
        Dyad[Boolean => Int](
          _ => 4,
          if (_) 20 else 18
        ),
        Dyad[Boolean => Int](
          if (_) 1 else 3,
          _ => 2
        )
      ) shouldBe false
      equalH(
        Dyad[Boolean => Int](_ => 4),
        Dyad[Boolean => Int](
          if (_) 4 else 4,
          b => (if (b) 8 else 9) / 2
        )
      ) shouldBe true
    }
    it("have Catcher nature") {
      val dyadIntCatcher =
        Catcher[Dyad[Int], Boolean, Int]

      dyadIntCatcher.either(
        Dyad(2)
      ) shouldBe Left(2)

      dyadIntCatcher.either(
        Dyad(2, 3)
      ) match {
        case Left(_) => fail("Unexpected singleton dyad")
        case Right(fn) =>
          dyadIntCatcher.either(fn(false)) shouldBe Left(2)
          dyadIntCatcher.either(fn(true)) shouldBe Left(3)
      }

      dyadIntCatcher.construct(Left(2)) shouldBe Dyad(2)
      dyadIntCatcher.construct(Right(boolean => if (boolean) Dyad(3) else Dyad(4, 5))) shouldBe Dyad(
        4,
        3,
        5,
        3
      )
    }
    it("act on Cantorians and other pitchers") {
      val dyad: Dyad[Int] =
        Dyad(2, 4, 7, 1, 9, 0, 3, 5)

      dyad(Cantorian.cycle(true)) shouldBe 5
      dyad(Cantorian.cycle(false)) shouldBe 2
      dyad(Cantorian.cycle(true, false)) shouldBe 0
      dyad(Cantorian.cycle(false, true)) shouldBe 7

      dyad(Pitcher.constantly[VanillaPitcher[Boolean], Boolean](true)) shouldBe 5
      dyad(Pitcher.constantly[VanillaPitcher[Boolean], Boolean](false)) shouldBe 2
    }
  }
}

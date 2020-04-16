package com.fdilke.bewl2.topology

import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.topology.EmptyEnumeration.Impossibility
import com.fdilke.bewl2.topology.Hausdorff._
import com.fdilke.bewl2.topology.StrontiumDogEnumeration._
import com.fdilke.bewl2.topology.WeekdayEnumeration._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.postfixOps

class TypeTopologyTest extends AnyFunSpec {

  private def preferredWeapon(sd: StrontiumDog): String =
    sd match {
      case Johnny   => "No.4 Cartridge"
      case Wulf     => "Der Happy Stick"
      case TheGronk => "Oh my poor heartses"
    }

  describe("Enumerations") {
    it("can be made implicitly compact") {
      determine[StrontiumDog] {
        preferredWeapon(_) == "Der Happy Stick"
      } shouldBe Some(Wulf)

      determine[StrontiumDog] {
        preferredWeapon(_) == "Time Bomb"
      } shouldBe None
    }

    it("allow use of quantifiers once implicitly compact") {
      exists[StrontiumDog] { sd =>
        preferredWeapon(sd).startsWith("No.4")
      } shouldBe true

      exists[StrontiumDog] { sd =>
        sd.toString.length < 3
      } shouldBe false

      forAll[StrontiumDog] { sd =>
        preferredWeapon(sd) contains ' '
      } shouldBe true

      forAll[StrontiumDog] { sd =>
        sd.toString.toCharArray.count(_.isUpper) == 1
      } shouldBe false

      StrontiumDogEnumeration.values should contain(optional[StrontiumDog].get)
      inhabited[StrontiumDog] shouldBe true
      optional[Impossibility] shouldBe None
      inhabited[Impossibility] shouldBe false
    }

    it("allow special helper method for testing intermediate results'") {
      determine[StrontiumDog, Int](
        sd => sd.id,
        _ > 1
      ) shouldBe Some(
        (TheGronk, 2)
      )
      determine[StrontiumDog, String](
        sd => sd.toString,
        _.length < 4
      ) shouldBe None
    }

    it("can be made implicitly Hausdorff") {
      equalH(Johnny, Wulf) shouldBe false
      equalH(TheGronk, TheGronk) shouldBe true
    }
  }

  describe("Inheritance of Hausdorff separation") {
    it("works from H to Seq[H]") {
      equalH[Seq[StrontiumDog]](
        Seq(Johnny, Wulf),
        Seq(Johnny, Wulf)
      ) shouldBe true

      equalH[Seq[StrontiumDog]](
        Seq(Johnny, Wulf),
        Seq(TheGronk)
      ) shouldBe false
    }
  }

  describe("Derived topologies") {
    it("Hausdorff ^ compact is implicitly Hausdorff") {
      equalH[StrontiumDog => Weekday](
        Map(
          Johnny -> Monday,
          Wulf -> Tuesday,
          TheGronk -> Sunday
        ),
        Map(
          Johnny -> Monday,
          Wulf -> Thursday,
          TheGronk -> Sunday
        )
      ) shouldBe false
      equalH[StrontiumDog => Weekday](
        Map(
          Johnny -> Monday,
          Wulf -> Tuesday,
          TheGronk -> Sunday
        ),
        Map(
          TheGronk -> Sunday,
          Wulf -> Tuesday,
          Johnny -> Monday
        )
      ) shouldBe true
    }
    it("Hausdorff ^ compact is implicitly Hausdorff - with H = Int") {
      equalH[StrontiumDog => Int](
        Map(
          Johnny -> 1,
          Wulf -> 2,
          TheGronk -> 0
        ),
        Map(
          Johnny -> 1,
          Wulf -> 4,
          TheGronk -> 0
        )
      ) shouldBe false
      equalH[StrontiumDog => Int](
        Map(
          Johnny -> 1,
          Wulf -> 2,
          TheGronk -> 0
        ),
        Map(
          TheGronk -> 0,
          Wulf -> 2,
          Johnny -> 1
        )
      ) shouldBe true
    }
    it("compact ^ Hausdorff is implicitly compact") {
      find[StrontiumDog => Weekday](specialDay => specialDay(Johnny) == Monday) match {
        case Some(prefunc) =>
          prefunc()(Johnny) shouldBe Monday
        case _ =>
          fail("No solution found")
      }
      find[StrontiumDog => Weekday] { specialDay =>
        specialDay(Wulf).id - specialDay(Johnny).id == 1
      } match {
        case Some(preSpecialDay) =>
          val specialDay: StrontiumDog => Weekday =
            preSpecialDay()
          specialDay(Wulf).id - specialDay(Johnny).id shouldBe 1
        case _ =>
          fail("No solution found")
      }
      find[StrontiumDog => Weekday] { specialDay =>
        specialDay(Wulf).id - specialDay(Johnny).id == 72
      } match {
        case Some(_) =>
          fail("Incorrect solution found")
        case _ =>
      }
    }
    it("compact ^ Hausdorff is implicitly compact - with Int") {
      find[Int => Weekday](specialDay => specialDay(3) == Monday) match {
        case Some(prefunc) =>
          prefunc()(3) shouldBe Monday
        case _ =>
          fail("No solution found")
      }
      find[Int => Weekday](specialDay => specialDay(7).id - specialDay(22).id == 1) match {
        case Some(preSpecialDay) =>
          val specialDay: Int => Weekday =
            preSpecialDay()
          specialDay(7).id - specialDay(22).id shouldBe 1
        case _ =>
          fail("No solution found")
      }
      find[Int => Weekday](specialDay => specialDay(88).id - specialDay(-100).id == 72) match {
        case Some(_) =>
          fail("Incorrect solution found")
        case _ =>
      }
    }
    it("compactness of compact ^ Hausdorff returns real functions") {
      val dogSeq: Int => StrontiumDog =
        (find[Int => StrontiumDog](dogSeq => dogSeq(3) == TheGronk) get)()
      dogSeq(3) shouldBe TheGronk
      StrontiumDogEnumeration.values should contain(dogSeq(77))
    }
    it("compactness of Boolean") {
      determine[Boolean] { b =>
        b
      } shouldBe Some(true)
      determine[Boolean](!_) shouldBe Some(false)
      determine[Boolean] { _ =>
        false
      } shouldBe None
    }
  }
}

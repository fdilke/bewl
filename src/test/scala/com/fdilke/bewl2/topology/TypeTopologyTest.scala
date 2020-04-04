package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl2.topology.Compact
import Compact._
import Hausdorff._
import StrontiumDogEnumeration._
import WeekdayEnumeration._
import com.fdilke.bewl2.topology.EmptyEnumeration.Impossibility

class TypeTopologyTest extends AnyFunSpec {

  private def preferredWeapon(sd: StrontiumDog): String =
    sd match {
      case Johnny => "No.4 Cartridge"
      case Wulf => "Der Happy Stick"
      case TheGronk => "Oh my poor heartses"
    }

  describe("Enumerations") {
    it("can be made implicitly compact") {
      find[StrontiumDog] {
        preferredWeapon(_) == "Der Happy Stick"
      } map {
        _()
      } shouldBe Some(Wulf)

      find[StrontiumDog] {
        preferredWeapon(_) == "Time Bomb"
      } map {
        _()
      } shouldBe None
    }

    it("allow use of quantifiers once implicitly compact") {
      exists[StrontiumDog] { sd =>
        preferredWeapon(sd) startsWith "No.4"
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

      inhabited[StrontiumDog] shouldBe true
      inhabited[Impossibility] shouldBe false
    }

    it("can be made implicitly Hausdorff") {
      equalH(Johnny, Wulf) shouldBe false
      equalH(TheGronk, TheGronk) shouldBe true
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
      find[StrontiumDog => Weekday] { specialDay =>
        specialDay(Johnny) == Monday
      } match {
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
  }
}

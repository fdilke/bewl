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
  }
}

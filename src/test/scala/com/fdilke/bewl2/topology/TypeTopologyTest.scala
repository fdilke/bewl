package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl2.topology.Compact
import Compact._
import Hausdorff._

class TypeTopologyTest extends AnyFunSpec {

  object WeekdayEnumeration extends Enumeration {
    val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
    type Weekday = Value
  }

  object StrontiumDogEnumeration extends Enumeration {
    val Johnny, Wulf, TheGronk = Value
    type StrontiumDog = Value
  }
  import StrontiumDogEnumeration._

  private def preferredWeapon(sd: StrontiumDog): String =
    sd match {
      case Johnny => "No.4 Cartridge"
      case Wulf => "Der Happy Stick"
      case TheGronk => "Oh my poor heartses"
    }

  describe("Enumerations") {
    it("can be made compact") {
      implicit val compactSD: Compact[StrontiumDog] =
        StrontiumDogEnumeration
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

    it("can be made Hausdorff") {
      implicit val hausdorffSD: Hausdorff[StrontiumDog] =
        StrontiumDogEnumeration

      equalH(Johnny, Wulf) shouldBe false
      equalH(TheGronk, TheGronk) shouldBe true
    }
  }

//  description("Hausdorff ^ compact is implicitly Hausdorff") {
//    it() {
//      find[Weekday => StrontiumDog]
//    }
//  }
}

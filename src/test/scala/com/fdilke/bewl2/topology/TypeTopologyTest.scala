package com.fdilke.bewl2.topology

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl2.topology.Compact
import Compact._
import Hausdorff._

class TypeTopologyTest extends AnyFunSpec {

  object Weekday extends Enumeration {
    val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
  }

  object SampleEnumeration extends Enumeration {
    val Johnny, Wulf, TheGronk = Value
    type StrontiumDog = Value
  }
  import SampleEnumeration._

  private def preferredWeapon(sd: StrontiumDog): String =
    sd match {
      case Johnny => "No.4 Cartridge"
      case Wulf => "Der Happy Stick"
      case TheGronk => "Oh my poor heartses"
    }

  describe("Enumerations") {
    it("can be made compact") {
      val csd = compactnessFor(SampleEnumeration)

      csd find {
        preferredWeapon(_) == "Der Happy Stick"
      } map {
        _()
      } shouldBe Some(Wulf)

      csd find {
        preferredWeapon(_) == "Time Bomb"
      } map {
        _()
      } shouldBe None
    }

    it("can be made Hausdorff") {
      val hsd = hausdorffFor(SampleEnumeration)

      hsd equal(Johnny, Wulf) shouldBe false
      hsd equal(TheGronk, TheGronk) shouldBe true
    }
  }
}

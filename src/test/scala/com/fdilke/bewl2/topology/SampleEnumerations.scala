package com.fdilke.bewl2.topology

object WeekdayEnumeration extends Enumeration {
  val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
  type Weekday = Value
}

object StrontiumDogEnumeration extends Enumeration {
  val Johnny, Wulf, TheGronk = Value
  type StrontiumDog = Value
  val NUM_STRONTIES: Int =
    values.size
}

object EmptyEnumeration extends Enumeration {
  type Impossibility = Value
}

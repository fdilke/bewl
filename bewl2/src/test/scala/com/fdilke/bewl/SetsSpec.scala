package com.fdilke.bewl

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.sets.Sets
import munit.FunSuite

class SetsSpec extends GenericToposTests()(Sets):

  import topos._

  override type FOO = String
  override implicit val dotFoo: Set[FOO] =
    Set("one", "two", "three")

  override type BAR = Boolean
  override implicit val dotBar: Set[Boolean] =
    Set(true, false)

  override type BAZ = Int
  override implicit val dotBaz: Set[Int] =
    Set(0, 1, 2, 3, 4, 5)

  override val foo2bar: String ~> Boolean =
    arrow[String, Boolean] { (x: String) =>
      true
    }

  override val foo2baz: String ~> Int =
    arrow[String, Int] { (x: String) =>
      x.length
    }



package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.GenericToposTests
import munit.FunSuite

class SetsSpec extends GenericToposTests()(Sets):

  import topos.*

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
    arrow[String, Boolean] { (_: String) =>
      true
    }

  override val foo2baz: String ~> Int =
    arrow[String, Int] { (text: String) =>
      text.length
    }

  override val foobar2baz: (String, Boolean) ~> Int =
    arrow[(String, Boolean), Int] {
      case (text, b) =>
        if b then
          text.length
        else
          text.length - 1
    }


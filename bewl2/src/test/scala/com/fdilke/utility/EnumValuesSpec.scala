package com.fdilke.utility

import com.fdilke.bewl2.utility.RichFunSuite
import com.fdilke.utility.Mask.*
import junit.framework.TestCase

import scala.runtime.Arrays
import scala.quoted.*

class EnumValuesSpec extends RichFunSuite:
  enum Suit:
    case Hearts, Diamonds, Clubs, Spades
  import Suit.*

  test("Can fetch the values from an enumeration") {
    EnumValues[Suit].toSeq is Seq[Suit](Hearts, Diamonds, Clubs, Spades)
  }

  test("Can fetch the values from an enumeration, via an abstraction layer") {
    inline def fancyFun[E]: Array[E] =
      EnumValues[E]

    fancyFun[Suit].toSeq is Seq[Suit](Hearts, Diamonds, Clubs, Spades)
  }



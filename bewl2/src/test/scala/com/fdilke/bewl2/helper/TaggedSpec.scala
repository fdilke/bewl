package com.fdilke.bewl2.helper

import munit.FunSuite
import munit.Clue.generate
import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._
import Tagged.*
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

class TaggedSpec extends FunSuite:

  test("Can tag and untag primitive types"):
    val obj: Int = 2
    val tagged: Int Tagged Boolean = obj.tag[Boolean]
    val untagged: Int = tagged.untag
    untagged is obj
    // val cast: Int = tagged.asInstanceOf[Int]
    // cast is obj

  test("Can tag and untag compound types"):
    case class Widget(degree: Int)
    val obj: Widget = Widget(2)
    val tagged: Widget Tagged List[Array[String]] = obj.tag[List[Array[String]]]
    val untagged: Widget = tagged.untag
    untagged is obj
    // val cast: Widget = tagged.asInstanceOf[Widget]
    // cast is obj




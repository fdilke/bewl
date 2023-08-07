package com.fdilke.bewl2.helper

import munit.FunSuite
import munit.Clue.generate
import com.fdilke.bewl2.utility.RichFunSuite
import Tagged.*
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

class TaggedSpec extends RichFunSuite:

  test("Can tag and untag primitive types"):
    val obj: Int = 2
    val tagged: Int Tagged Boolean = obj.tag[Boolean]
    val untagged: Int = tagged.untag
    untagged is obj

  test("Can tag and untag compound types"):
    case class Widget(degree: Int)
    val obj: Widget = Widget(2)
    val tagged: Widget Tagged List[Array[String]] = obj.tag[List[Array[String]]]
    val untagged: Widget = tagged.untag
    untagged is obj




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




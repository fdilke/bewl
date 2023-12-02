package com.fdilke.bewl2.sets.morphenum

import munit.FunSuite
import com.fdilke.bewl2.utility.RichFunSuite._

class CullRelatorsSpec extends FunSuite:
  val Seq(x, y, z) =
    Seq[String]("x", "y", "z").map { Symbol(_) }

  test("Culling relators preserves irredundant sets"):
    checkSameElementsAs(
      CullRelators(
        1,
        Seq(
          Relator(x, 0, y)
        )
      ),
      Seq(
        Relator(x, 0, y)
      )
    )

    test("Culling relators removes diagonal entries"):
      checkSameElementsAs(
        CullRelators(
          1,
          Seq(
            Relator(x, 0, y),
            Relator(z, 1, z),
            Relator(y, 0, y)
          )
        ),
        Seq(
          Relator(x, 0, y),
          Relator(y, 0, y)
        )
      )
    
    test("Culling relators removes symmetric entries"):
      checkSameElementsAs(
        CullRelators(
          1,
          Seq(
            Relator(x, 0, y),
            Relator(x, 1, z),
            Relator(y, 0, x),
            Relator(z, 1, x),
            Relator(z, 1, y)
          )
        ),
        Seq(
          Relator(x, 0, y),
          Relator(x, 1, z),
          Relator(y, 0, x),
          Relator(z, 1, y)
        )
      )

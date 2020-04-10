package com.fdilke.bewl.fsets.monoid_actions

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import com.fdilke.bewl.helper.StandardSymbols.{x, y, z}

class CullRelatorsTest extends AnyFreeSpec {
  "Culling relators" - {
    "preserves irredundant sets" in {
      CullRelators(
        1,
        Seq(
          Relator(x, 0, y)
        )
      ) should contain theSameElementsAs Seq(
        Relator(x, 0, y)
      )
    }

    "removes diagonal entries" in {
      CullRelators(
        1,
        Seq(
          Relator(x, 0, y),
          Relator(z, 1, z),
          Relator(y, 0, y)
        )
      ) should contain theSameElementsAs Seq(
        Relator(x, 0, y),
        Relator(y, 0, y)
      )
    }

    "removes symmetric entries" in {
      CullRelators(
        1,
        Seq(
          Relator(x, 0, y),
          Relator(x, 1, z),
          Relator(y, 0, x),
          Relator(z, 1, x),
          Relator(z, 1, y)
        )
      ) should contain theSameElementsAs Seq(
        Relator(x, 0, y),
        Relator(x, 1, z),
        Relator(y, 0, x),
        Relator(z, 1, y)
      )
    }
  }
}

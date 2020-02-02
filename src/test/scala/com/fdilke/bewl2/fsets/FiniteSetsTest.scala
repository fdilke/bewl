package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.topos.{GenericToposTests, ToposFixtures}
import FiniteSets._

class FiniteSetsTest extends GenericToposTests[Set](
  new ToposFixtures[Set] {
  }
) {
}

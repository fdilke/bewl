package com.fdilke.bewl.topos

object StarTag extends Enumeration {
  class StarTag[SAMETYPE] {
    type TYPE
  }

  class Principal
  val principal = new StarTag[Principal] {
    override type TYPE = Principal
  }

  class RightScalar
  val rightScalar = new StarTag[RightScalar] {
    override type TYPE = RightScalar
  }
}

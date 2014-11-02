package com.fdilke.bewl.topos

// TODO: can simplify? should StarTag[TAG_TYPE] just be TAG_TYPE?

object StarTag extends Enumeration {
  class StarTag[TAG_TYPE]

  class Principal
  val principal = new StarTag[Principal]

  class RightScalar
  val rightScalar = new StarTag[RightScalar]
}

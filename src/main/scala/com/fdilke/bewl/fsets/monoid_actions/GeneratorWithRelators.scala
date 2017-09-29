package com.fdilke.bewl.fsets.monoid_actions

case class GeneratorWithRelators[M, A](
  generator: A,
  relators: Seq[Relator[M]]
)

case class Relator[M](
  selfScalar: M,
  otherIndex: Int,
  otherScalar: M
)


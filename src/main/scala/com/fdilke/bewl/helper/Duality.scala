package com.fdilke.bewl.helper

case class Duality[A, B] (
  / : A => B,
  \ : B => A
)

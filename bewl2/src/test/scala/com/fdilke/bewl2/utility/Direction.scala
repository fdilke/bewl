package com.fdilke.bewl2.utility

// Handy for testing in situations where we need a two-instance class that doesn't
// have an implicit Set[], and so can't use Boolean

sealed trait Direction

object Direction:
  case object Up extends Direction
  case object Down extends Direction


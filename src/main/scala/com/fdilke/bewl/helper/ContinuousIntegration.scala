package com.fdilke.bewl.helper

object ContinuousIntegration {

  def notOnSnap(
    function: => Unit
  ) =
    if (Option(System.getenv("SNAP_CI")).isEmpty)
      function
}

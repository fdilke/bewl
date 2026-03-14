package com.fdilke.bewl.helper

object ContinuousIntegration {

  private val ciSpecificSettings =
    Seq(
      "CI", // for CircleCI
      "SNAP_CI" // for SnapCI
    )

  def notOnCI(
    function: => Unit
  ) =
    if (!ciSpecificSettings.exists(setting => Option(System.getenv(setting)).isEmpty))
      function
}

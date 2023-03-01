package com.fdilke.utility

object TimeIt {
  def apply[H](block: => H): (H, String) =
    val start = System.currentTimeMillis()
    val result: H = block
    val end = System.currentTimeMillis()
    val runTime = (end - start)
    val stringRep = runTime match {
      case 0 => "-"
      case _ if runTime < 1000 => s"${runTime}ms"
      case _ if runTime < 60000 => 
        val numSec = runTime / 1000.0
        s"${numSec} sec"
      case _ if runTime < 3600000 => 
        val numMin = runTime / 60000.0
        s"${numMin} min"
      case _ =>
        val numHours = runTime / 3600000.0
        s"${numHours} hours"
    }
    (result, stringRep)
}

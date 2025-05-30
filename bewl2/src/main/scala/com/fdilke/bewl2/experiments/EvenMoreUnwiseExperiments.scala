package com.fdilke.bewl2.experiments

object EvenMoreUnwiseExperiments extends App:
//  def noddy4(
//              block: [E] => Sets.Dot[E] ?=> (monoid: Sets.Monoid[E]) ?=> monoid.Action[Int] ?=> Unit
//            ): Unit = ()
//  noddy4 {
//    [E] => (_ : Sets.Dot[E]) ?=> (monoid: Sets.Monoid[E]) ?=> (action: monoid.Action[Int]) ?=>
//      ()
//  }
  def noddy4(
    block: [E] => List[E] ?=> Int
  ): Unit =
    given List[String] =
      List("xx", "yy", "zz")
    val measure = block[String]
    println(s"measure = $measure")
  noddy4 {
    [E] => (list: List[E]) ?=> 
      list.size
  }
  


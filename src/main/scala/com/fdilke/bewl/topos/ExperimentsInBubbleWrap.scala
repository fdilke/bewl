package com.fdilke.bewl.topos

import scala.language.higherKinds
import scala.language.existentials
import scala.language.reflectiveCalls

object ExperimentsInBubbleWrap {
  case class PreWidget[T](t: T) { def asWidget = Widget(t) }
  case class Widget[T](t: T)
  case class PreWidgetArrow[T, U](f: T => U) { def asWidgetArrow = WidgetArrow(f) }
  case class WidgetArrow[T, U](f: T => U)
  case class PreDoodadArrow[T, U](f: T => U) { def asDoodadArrow = DoodadArrow(f) }
  case class DoodadArrow[T, U](f: T => U)

  trait PseudoTopos {
    type ~
    type DOT[T <: ~]
    type ARROW[T <: ~, U <: ~]
  }
  trait Wrappings[BASE, PREDOT[_ <: BASE], PREARROW[_ <: BASE, _ <: BASE]] { topos: PseudoTopos =>
    type WRAPPER[T <: BASE] <: ~
    def makeDot[T <: BASE](predot: PREDOT[T]) : DOT[WRAPPER[T]]
    def makeArrow[T <: BASE, U <: BASE](prearrow: PREARROW[T, U]) : ARROW[WRAPPER[T], WRAPPER[U]]
  }
  object FiniteSets extends PseudoTopos with Wrappings[Any, PreWidget, PreWidgetArrow]{
    override type ~ = Any
    override type DOT[T] = Widget[T]
    override type ARROW[T, U] = WidgetArrow[T, U]
    override type WRAPPER[T] = T

    override def makeDot[T <: Any](predot: PreWidget[T]) =
      predot.asWidget
    override def makeArrow[T <: Any, U <: Any](prearrow: PreWidgetArrow[T, U]) =
      prearrow.asWidgetArrow

    val sampleAuto: ARROW[Int, Int] =
      WidgetArrow({ x => x })
  }
  class AutoContext[BASE, PREDOT[_ <: BASE], PREARROW[_ <: BASE, _ <: BASE]](
    val Ɛ : PseudoTopos with Wrappings[BASE, PREDOT, PREARROW]
  ) {
    case class Doodad[T <: Ɛ.~](t: Ɛ.ARROW[T, T])

    val build = new PseudoTopos with Wrappings[
      Ɛ.~,
      ({type λ[X <: Ɛ.~] = Ɛ.ARROW[X, X]})#λ,
      PreDoodadArrow
    ] {
      override type ~ = Ɛ.~
      override type DOT[T <: Ɛ.~] = Doodad[T]
      override type ARROW[T <: Ɛ.~, U <: Ɛ.~] = DoodadArrow[T, U]
      override type WRAPPER[T <: Ɛ.~] = T

      override def makeDot[T <: Ɛ.~](predot: Ɛ.ARROW[T, T]) =
        Doodad(predot)

      override def makeArrow[T <: Any, U <: Any](prearrow: PreDoodadArrow[T, U]) =
        prearrow.asDoodadArrow
    }
  }
  val context: AutoContext[Any, PreWidget, PreWidgetArrow] =
    new AutoContext[Any, PreWidget, PreWidgetArrow](FiniteSets)
  val permutations = context.build
//  val sampleAuto: context.Ɛ.ARROW[Int, Int] = context.Ɛ.sampleAuto
  val prearrow: PreWidgetArrow[Int, Int] = ???
//  val sampleAuto: context.Ɛ.ARROW[Int, Int] = context.Ɛ.makeArrow(prearrow)
//  val hh = context.Ɛ.makeArrow(null)
//  permutations.makeDot(sampleAuto)
}

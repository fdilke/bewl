package com.fdilke.bewl.topos

import scala.language.higherKinds

object ExperimentsInBubbleWrap {
  case class PreWidget[T](t: T) { def asWidget = Widget(t) }
  case class Widget[T](t: T)
  case class PreWidgetArrow[T, U](f: T => U) { def asWidgetArrow = WidgetArrow(f) }
  case class WidgetArrow[T, U](f: T => U)
  case class PreDoodad[T](t: T) { def asDoodad = Doodad(t) }
  case class Doodad[T](t: T)
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
  }
  def automorphisms(Ɛ: PseudoTopos) =
    new PseudoTopos with Wrappings[Ɛ.~, PreDoodad, PreDoodadArrow] {
      override type ~ = Ɛ.~
      override type DOT[T] = Doodad[T]
      override type ARROW[T, U] = DoodadArrow[T, U]
      override type WRAPPER[T <: Ɛ.~] = T

      override def makeDot[T <: Any](predot: PreDoodad[T]) =
        predot.asDoodad
      override def makeArrow[T <: Any, U <: Any](prearrow: PreDoodadArrow[T, U]) =
        prearrow.asDoodadArrow
    }

  val permutations = automorphisms(FiniteSets)
}

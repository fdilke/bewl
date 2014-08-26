package com.fdilke.bewl.helper

case class StrictRef[+X <: AnyRef](wrappedValue: X) {
  override def equals(other: scala.Any) = other match {
    case other: StrictRef[X] => wrappedValue eq other.wrappedValue
  }

  override def hashCode = wrappedValue.hashCode
}

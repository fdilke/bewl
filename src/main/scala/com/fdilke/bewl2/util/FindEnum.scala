package com.fdilke.bewl2.util

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.language.{implicitConversions, postfixOps}

object FindEnum {
  def apply[
    ENUM <: Enumeration : TypeTag
  ]: ENUM = {
    val enumType: ru.Type = typeOf[ENUM]
    val className = enumType.typeSymbol.asClass.fullName + "$"
    getClass.getClassLoader.loadClass(className).getField("MODULE$").get(null).asInstanceOf[ENUM]
  }
}
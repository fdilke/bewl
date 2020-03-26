package com.fdilke.bewl2.util

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.language.{implicitConversions, postfixOps}

object FindSingleton {
  def apply[
    SINGLETON : TypeTag
  ]: SINGLETON = {
    val enumType: ru.Type = typeOf[SINGLETON]
    val className = enumType.typeSymbol.asClass.fullName + "$"
    getClass.getClassLoader.loadClass(className).getField("MODULE$").get(null).asInstanceOf[SINGLETON]
  }
}
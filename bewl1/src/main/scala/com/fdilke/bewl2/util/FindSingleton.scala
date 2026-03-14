package com.fdilke.bewl2.util

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.language.{implicitConversions, postfixOps}

object FindSingleton {
  def apply[
    SINGLETON: TypeTag
  ]: SINGLETON = {
    val enumType: ru.Type = typeOf[SINGLETON]
    if (enumType.isInstanceOf[SingletonTypeApi]) {
      val className = enumType.typeSymbol.asClass.fullName + "$"
      getClass.getClassLoader
        .loadClass(className)
        .getField("MODULE$")
        .get(null)
        .asInstanceOf[SINGLETON]
    } else
      throw new IllegalArgumentException(
        enumType.typeSymbol.toString + " is not a singleton"
      )
  }
}

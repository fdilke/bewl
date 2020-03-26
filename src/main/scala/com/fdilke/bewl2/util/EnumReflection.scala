package com.fdilke.bewl2.util

import scala.reflect.runtime.universe._
import scala.language.{implicitConversions, postfixOps}
import scala.reflect.ClassTag

object EnumReflection {
  import scala.reflect.runtime.{universe => ru}

  def enumContainerFor[T: ru.TypeTag]  = {
    val enumType: ru.Type = typeOf[T].asInstanceOf[TypeRef].pre
    val className = enumType.typeSymbol.asClass.fullName + "$"
    getClass.getClassLoader.loadClass(className).getField("MODULE$").get(null)
  }
}
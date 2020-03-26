package com.fdilke.bewl2.topology

import Compact._

import scala.reflect.runtime.universe._
import scala.language.{implicitConversions, postfixOps}
import scala.reflect.ClassTag

trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]
}

object ReflectionSugars{
  import scala.reflect.runtime.{universe => ru}
//  private lazy val universeMirror = ru.runtimeMirror(getClass.getClassLoader)
//
//  def companionOf[T](implicit tt: ru.TypeTag[T])  = {
//    val companionMirror = universeMirror.reflectModule(ru.typeOf[T].typeSymbol.companion.asModule)
//    companionMirror.instance.asInstanceOf[T]
//  }

  private val runtimeMirror: ru.Mirror =
      ru.runtimeMirror(getClass.getClassLoader)

//  def s_companionOf[T](implicit tt: ru.SingletonType[T])  = {
//    tt.
//  }

  def enumContainerFor[T](implicit tt: ru.TypeTag[T])  = {
    val enumElementType: Type = typeOf[T]

    val tref = enumElementType.asInstanceOf[TypeRef]
//    val tref = enumElementType.asInstanceOf[scala.reflect.internal.Types#UniqueSingleType]
    val enumType /*: Type */ = tref.pre
    val className = enumType.typeSymbol.asClass.fullName + "$"
    println("VVV classname = [" + className + "]")
    /// The enum's companion object
    getClass.getClassLoader.loadClass(className)


    val enumObject = getClass.getClassLoader.loadClass(className).getField("MODULE$").get(null)
    println("VVV enumObject = " + enumObject)
    enumObject
    }

//  def companionOf[T](implicit tt: ru.TypeTag[T])  = {
//    val companionMirror =
//      runtimeMirror.reflect(this).reflectModule(ru.typeOf[T].typeSymbol.companionSymbol.asModule)
//    companionMirror.instance
//  }

  //  def companionOf[T](implicit tt: ru.TypeTag[T])  = {
//    val module: ru.ModuleSymbol = tt.mirror.staticModule(tt. // tt.getClass.getName)
//    runtimeMirror.reflectModule(module).instance
//  }

//  implicit val NameClassMap: Map[String, Class[_ <: Attribute[_]]] = (concreteAttributeClasses map { klass =>
//    val module = runtimeMirror.staticModule(klass.getName)
//    val companionObj = runtimeMirror.reflectModule(module).instance.asInstanceOf[OrgNamed]
//    companionObj.orgName -> klass
}

object Compact {

  implicit def CompactnessForEnum[
    ENUM <: Enumeration : TypeTag
  ]: Compact[ENUM#Value] = {
    val enum: ENUM = ReflectionSugars.enumContainerFor[ENUM#Value].asInstanceOf[ENUM]
    println("VVV found enum:" +  enum.getClass.getSimpleName )

//        val tt = typeTag[ENUM]
//        println("VVV found class:" +  clazz.getSimpleName )
//        val clazz = tt.mirror.classLoader.getClass
//        println("VVV found class:" +  clazz.getSimpleName )
//        tt.getClass.
    (predicate: ENUM#Value => Boolean) =>
      enum.values find (predicate) map {
        v => () => v
      }
  }

  @inline def find[T : Compact](
    predicate: T => Boolean
  ): Option[
    () => T
  ] =
    implicitly[Compact[T]] find(
      predicate
    )

  @inline def exists[T : Compact](
    predicate: T => Boolean
  ): Boolean =
    find[T](
      predicate
    ) isDefined

  @inline def forAll[T : Compact](
    predicate: T => Boolean
  ): Boolean =
    ! exists[T] {
      !predicate(_)
    }
}

trait Hausdorff[T] {
  def equalH(
    t1: T,
    t2: T
  ): Boolean
}

object Hausdorff {
  implicit def HausdorffForEnum[ENUM <: Enumeration](
    enum: ENUM
  ): Hausdorff[enum.Value] =
      _ == _

  @inline def equalH[T : Hausdorff](
    t1: T,
    t2: T
  ): Boolean =
    implicitly[Hausdorff[T]] equalH(
      t1, t2
    )

  implicit def hausdorffExponential[
    C : Compact,
    H : Hausdorff
  ]: Hausdorff[
    C => H
  ] =
    (f, g) =>
      forAll[C] { c =>
        equalH(
          f(c),
          g(c)
        )
      }
}

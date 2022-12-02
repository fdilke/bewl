package com.fdilke.utility

import scala.compiletime.{constValue, constValueTuple}
import scala.deriving.Mirror
import scala.reflect.{ClassTag, classTag}
import scala.quoted.*

object EnumValues:
  inline def apply[E]: Array[E] =
    ${enumValuesImpl[E]}

//  inline def enumDescription[E](using m: Mirror.SumOf[E]): String =
//    val name = constValue[m.MirroredLabel]
//    val values = constValueTuple[m.MirroredElemLabels].productIterator.mkString(", ")
//    s"$name: $values"

  def enumValuesImpl[E: Type](using Quotes): Expr[Array[E]] =
    import quotes.reflect.*
    val companion = Ref(TypeTree.of[E].symbol.companionModule)
    Select.unique(companion, "values").asExprOf[Array[E]]



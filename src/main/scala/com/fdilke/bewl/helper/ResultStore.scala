package com.fdilke.bewl.helper

class ResultStore[K, V](f: K => V) {
  private val resultMap = scala.collection.mutable.Map[K, V]()
  def apply(key: K): V =
    resultMap.getOrElseUpdate(key, f(key))
}

package com.fdilke.bewl.helper

// User: Felix Date: 05/06/2014 Time: 18:13

class ResultStore[K, V](f: K => V) {
  private val standardProducts = scala.collection.mutable.Map[K, V]()
  def apply(key: K): V =
    standardProducts.getOrElseUpdate(key, f(key))
}

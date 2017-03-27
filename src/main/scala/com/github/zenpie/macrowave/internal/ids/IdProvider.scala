package com.github.zenpie.macrowave.internal.ids

import scala.collection.mutable

final class IdProvider[@specialized T <: Id](f: Int => T) {

  private var counter: Int = 0

  def next(): T = {
    val current = f(counter)
    counter += 1
    current
  }

  sealed trait Table[U] {
    def apply(key: T): U
    def update(key: T, value: U): Unit
  }

  def model[U](default: T => U): Table[U] = new Table[U] {
    private val data = mutable.ArrayBuffer[U]()

    private def ensure(key: T): Int = {
      val index = key.value
      while (index >= data.size) data += default(key)
      index
    }

    def apply(key: T): U =
      data(ensure(key))

    def update(key: T, value: U): Unit =
      data(ensure(key)) = value

  }

}

package com.github.zenpie.macrowave.internal.ids

import scala.collection.mutable

sealed trait Table[T <: Id, U] {

  def apply(key: T): U

  def update(key: T, value: U): Unit

}

final class IdProvider[T <: Id](f: Int => T) {

  private var counter: Int = 0

  def next(): T = {
    val current = f(counter)
    counter += 1
    current
  }

  def dataTable[U](default: T => U): Table[T, U] = new Table[T, U] {
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

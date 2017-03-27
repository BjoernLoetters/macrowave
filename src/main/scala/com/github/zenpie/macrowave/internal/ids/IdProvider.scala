package com.github.zenpie.macrowave.internal.ids

class IdProvider[@specialized T <: Id](f: Int => T) {

  private var counter: Int = 0

  def next(): T = {
    val current = f(counter)
    counter += 1
    current
  }

}

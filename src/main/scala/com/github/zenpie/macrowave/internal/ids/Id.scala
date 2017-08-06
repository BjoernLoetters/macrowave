package com.github.zenpie.macrowave.internal.ids

trait Id extends Product with Serializable {
  val value: Int
}

package com.github.zenpie.macrowave.internal.datalift

/**
  * Reference to a tree, which yields a value of type `T` at runtime.
  */
case class TreeRef[Tree, T] private[datalift] (private val id: Int)

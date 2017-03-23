package com.github.zenpie.macrowave

import scala.annotation.compileTimeOnly

sealed abstract class RegExp {

  @compileTimeOnly("Calls to method '~' have to be inside a macro invocation!")
  def ~(other: RegExp): RegExp = compileTime

  @compileTimeOnly("Calls to method '|' have to be inside a macro invocation!")
  def |(other: RegExp): RegExp = compileTime


  @compileTimeOnly("Calls to method '*' have to be inside a macro invocation!")
  def * : RegExp = compileTime

  @compileTimeOnly("Calls to method '+' have to be inside a macro invocation!")
  def + : RegExp = compileTime

  @compileTimeOnly("Calls to method '?' have to be inside a macro invocation!")
  def ? : RegExp = compileTime

}

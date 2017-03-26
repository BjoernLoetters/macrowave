package com.github.zenpie.macrowave

import scala.annotation.compileTimeOnly

sealed abstract class Rule[+T <: HList] {

  @compileTimeOnly("Calls to method '~' have to be inside a macro invocation!")
  def ~[U <: HList, V >: T <: HList](other: Rule[U])(implicit P: Prepend[V, U]): Rule[P.Out] = compileTime

  @compileTimeOnly("Calls to method '|' have to be inside a macro invocation!")
  def |[U >: T <: HList](other: Rule[U]): Rule[U] = compileTime


  @compileTimeOnly("Calls to method '*' have to be inside a macro invocation!")
  def *[V >: T <: HList] : Rule[List[V] :: HNil] = compileTime

  @compileTimeOnly("Calls to method '+' have to be inside a macro invocation!")
  def +[V >: T <: HList] : Rule[V :: List[V] :: HNil] = compileTime

  @compileTimeOnly("Calls to method '?' have to be inside a macro invocation!")
  def ?[V >: T <: HList] : Rule[Option[V] :: HNil] = compileTime

}

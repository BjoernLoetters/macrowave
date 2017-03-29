package com.github.zenpie.macrowave

import scala.annotation.compileTimeOnly

sealed abstract class Rule[+T <: HList] {

  @compileTimeOnly("Calls to method '~' have to be inside a macro invocation!")
  def ~[U <: HList, V >: T <: HList](other: Rule[U])(implicit P: Prepend[V, U]): Rule[P.Out] = compileTime

  @compileTimeOnly("Calls to method '|' have to be inside a macro invocation!")
  def |[U >: T <: HList](other: Rule[U]): Rule[U] = compileTime


  @compileTimeOnly("Calls to method '*' have to be inside a macro invocation!")
  def *[V >: T <: HList](implicit S: SingletonRule[V]) : Rule[List[S.Out] :: HNil] = compileTime

  @compileTimeOnly("Calls to method '+' have to be inside a macro invocation!")
  def +[V >: T <: HList](implicit S: SingletonRule[V]) : Rule[S.Out :: List[S.Out] :: HNil] = compileTime

  @compileTimeOnly("Calls to method '?' have to be inside a macro invocation!")
  def ?[V >: T <: HList](implicit S: SingletonRule[V]) : Rule[Option[S.Out] :: HNil] = compileTime

}

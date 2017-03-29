package com.github.zenpie

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions
import scala.util.matching.Regex

package object macrowave extends RuleActions {

  type HList = shapeless.HList
  type ::[H, T <: HList] = shapeless.::[H, T]

  type HNil = shapeless.HNil
  val HNil = shapeless.HNil

  type Prepend[P <: HList, S <: HList] = shapeless.ops.hlist.Prepend[P, S]

  type Rule1[+T] = Rule[T :: HNil]


  def compileTime: Nothing = throw new UnsupportedOperationException("")


  @compileTimeOnly("Calls to function 'token' have to be inside a macro invocation!")
  implicit def token(regex: RegExp): Token = compileTime

  @compileTimeOnly("Calls to function 'singletonRule' have to be inside a macro invocation!")
  implicit def singletonRule(token: Token): Rule1[String] = compileTime


  @compileTimeOnly("Calls to function 'literal' have to be inside a macro invocation!")
  implicit def literal(string: String): RegExp = compileTime

  @compileTimeOnly("Calls to function 'regex' have to be inside a macro invocation!")
  def regex(string: String): RegExp = compileTime

  @compileTimeOnly("Calls to function 'regex' have to be inside a macro invocation!")
  implicit def regex(regex: Regex): RegExp = compileTime

}

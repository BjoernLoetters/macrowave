package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.ids.{ActionId, TypeId}

sealed trait Rule {
  val tpe: TypeId
}

case class Concatenate(left: Rule, right: Rule, tpe: TypeId) extends Rule
case class Alternate(left: Rule, right: Rule, tpe: TypeId) extends Rule
case class PClosure(rule: Rule, tpe: TypeId) extends Rule
case class Kleene(rule: Rule, tpe: TypeId) extends Rule
case class Optional(rule: Rule, tpe: TypeId) extends Rule
case class Transform(rule: Rule, action: ActionId, tpe: TypeId) extends Rule
case class Terminal(name: String, tpe: TypeId) extends Rule
case class NonTerminal(name: String, tpe: TypeId) extends Rule
case class Epsilon(tpe: TypeId) extends Rule

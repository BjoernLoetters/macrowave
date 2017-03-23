package com.github.zenpie.macrowave.internal.scanner

sealed trait Rule

case class Concatenate(left: Rule, right: Rule) extends Rule
case class Alternate(left: Rule, right: Rule) extends Rule
case class Range(from: Char, to: Char) extends Rule
case class Kleene(rule: Rule) extends Rule
case class Optional(rule: Rule) extends Rule

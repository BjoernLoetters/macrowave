package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.Grammar
import com.github.zenpie.macrowave.internal.ids.{NonTerminalId, TerminalId}

sealed trait Symbol {
  def show(implicit grammar: Grammar): String
}
sealed trait TerminalSymbol extends Symbol

case class TokenSymbol(terminalId: TerminalId) extends TerminalSymbol {
  override def equals(other: Any): Boolean = other match {
    case TokenSymbol(o) => terminalId == o
    case _              => false
  }
  override def hashCode(): Int = 1
  def show(implicit grammar: Grammar): String =
    grammar.terminalNames(terminalId)
}

case object EpsilonSymbol extends TerminalSymbol {
  def show(implicit grammar: Grammar): String = "ε"
}

case object EofSymbol extends TerminalSymbol {
  def show(implicit grammar: Grammar): String = "<eof>"
}

case class NonTerminalSymbol(nonTerminalId: NonTerminalId) extends Symbol {
  def show(implicit grammar: Grammar): String =
    grammar.nonTerminalNames(nonTerminalId)
}

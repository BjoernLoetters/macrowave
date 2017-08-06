package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.ids.{NonTerminalId, TerminalId}

sealed trait Symbol
sealed trait TerminalSymbol extends Symbol

case class TokenSymbol(terminalId: TerminalId) extends TerminalSymbol {
  override def equals(other: Any): Boolean = other match {
    case TokenSymbol(o) => terminalId == o
    case _              => false
  }
  override def hashCode(): Int = 1
  override def toString = s"Token($terminalId)"
}

case object EpsilonSymbol extends TerminalSymbol {
  override def toString = "ε"
}

case object EofSymbol extends TerminalSymbol {
  override def toString = "<eof>"
}

case class NonTerminalSymbol(nonTerminalId: NonTerminalId) extends Symbol {
  override def toString = s"NonTerminal($nonTerminalId)"
}

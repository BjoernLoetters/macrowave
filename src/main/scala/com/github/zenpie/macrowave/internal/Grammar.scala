package com.github.zenpie.macrowave.internal

import com.github.zenpie.macrowave.internal.ids.{ActionId, NonTerminalId, TerminalId, TypeId}

import scala.collection.mutable
import scala.reflect.macros.whitebox

final class Grammar(val c: whitebox.Context) {

  /* IDs */

  private[internal] val actionIdProvider = new IdProvider[ActionId](new ActionId(_))
  private[internal] val typeIdProvider = new IdProvider[TypeId](new TypeId(_))
  private[internal] val nonTerminalIdProvider = new IdProvider[NonTerminalId](new NonTerminalId(_))
  private[internal] val terminalIdProvider = new IdProvider[TerminalId](new TerminalId(_))

  private[internal] val actionMapping = mutable.Map[ActionId, c.universe.Tree]()
  private[internal] val typeMapping = mutable.Map[TypeId, c.universe.Tree]()

  /* non-terminals */

  private[internal] val namedNonTerminals = mutable.Map[String, NonTerminalId]()
  private[internal] val nonTerminalNames  = mutable.Map[NonTerminalId, String]()
  private[internal] val nonTerminals = mutable.Map[NonTerminalId, parser.Rule]()

  private[internal] var startRule: NonTerminalId = _

  /* terminals */

  private[internal] val namedTerminals = mutable.Map[String, TerminalId]()
  private[internal] val terminalNames = mutable.Map[TerminalId, String]()
  private[internal] val terminals = mutable.Map[TerminalId, scanner.Rule]()

}

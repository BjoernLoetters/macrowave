package com.github.zenpie.macrowave.internal

import com.github.zenpie.macrowave.internal.ids.{ActionId, NonTerminalId, TerminalId, TypeId}

import scala.collection.mutable
import scala.reflect.macros.blackbox

final class Grammar(val c: blackbox.Context) {

  /* IDs */

  private[internal] val actionIdProvider = new IdProvider[ActionId](new ActionId(_))
  private[internal] val typeIdProvider = new IdProvider[TypeId](new TypeId(_))
  private[internal] val nonTerminalIdProvider = new IdProvider[NonTerminalId](new NonTerminalId(_))
  private[internal] val terminalIdProvider = new IdProvider[TerminalId](new TerminalId(_))

  private[internal] val actionMapping = mutable.Map[ActionId, c.Tree]()
  private[internal] val typeMapping = mutable.Map[TypeId, c.Tree]()
  private[internal] val nonTerminalMapping = mutable.Map[String, NonTerminalId]()

  /* terminals */

  private[internal] val namedTerminals = mutable.Map[String, TerminalId]()
  private[internal] val terminals = mutable.Map[TerminalId, scanner.Rule]()

}

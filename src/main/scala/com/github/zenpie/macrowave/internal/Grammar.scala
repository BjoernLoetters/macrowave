package com.github.zenpie.macrowave.internal

import com.github.zenpie.macrowave.internal.parser.{ActionId, NonTerminalId, TypeId}
import com.github.zenpie.macrowave.internal.scanner.TerminalId

import scala.collection.mutable
import scala.reflect.macros.blackbox

final class Grammar(val c: blackbox.Context) {
  private[internal] val actionMapping = mutable.Map[ActionId, c.Tree]()
  private[internal] val typeMapping = mutable.Map[TypeId, c.Tree]()
  private[internal] val nonTerminalMapping = mutable.Map[String, NonTerminalId]()
  private[internal] val terminalMapping = mutable.Map[String, TerminalId]()

}

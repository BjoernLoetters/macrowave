package com.github.zenpie.macrowave.internal

import com.github.zenpie.macrowave.internal.ids.{ActionId, NonTerminalId, TerminalId, TypeId}

import scala.collection.mutable
import scala.reflect.macros.whitebox

final class Grammar(val c: whitebox.Context) {

  private[internal] type Tree = c.universe.Tree
  private[internal] type Position = c.universe.Position

  /* IDs */

  private[internal] val actionIdProvider = new IdProvider[ActionId](new ActionId(_))
  private[internal] val typeIdProvider = new IdProvider[TypeId](new TypeId(_))
  private[internal] val nonTerminalIdProvider = new IdProvider[NonTerminalId](new NonTerminalId(_))
  private[internal] val terminalIdProvider = new IdProvider[TerminalId](new TerminalId(_))

  /* positions */

  private[internal] val nonTerminalPositions = mutable.Map[NonTerminalId, Position]()

  /* actions */

  private[internal] val actions = mutable.Map[ActionId, Tree]()

  /* types of nonTerminals */

  private[internal] val types = mutable.Map[TypeId, Tree]()

  /* non-terminals */

  private[internal] val namedNonTerminals = mutable.Map[String, NonTerminalId]()
  private[internal] val nonTerminalNames  = mutable.Map[NonTerminalId, String]()
  private[internal] val nonTerminals = mutable.Map[NonTerminalId, parser.Rule]()

  private[internal] var startRule: NonTerminalId = _

  /* terminals */

  private[internal] val namedTerminals = mutable.Map[String, TerminalId]()
  private[internal] val terminalNames = mutable.Map[TerminalId, String]()

  private[internal] var whiteSpace = Option.empty[TerminalId]

  /* auxiliary definitions */

  private[internal] val terminals = mutable.Map[TerminalId, scanner.Rule]()
  private[internal] val auxiliaryDefs = mutable.ArrayBuffer[Tree]()

}

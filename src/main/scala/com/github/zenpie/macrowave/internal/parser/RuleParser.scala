package com.github.zenpie.macrowave.internal.parser

import java.util.LinkedList

import com.github.zenpie.macrowave.internal.ids.NonTerminalId
import com.github.zenpie.macrowave.internal._

import scala.collection.mutable
import scala.reflect.macros.whitebox

trait RuleParser extends MacroUtils {
  val c: whitebox.Context
  import c.universe._

  private[internal] def parserRulesFromStatements(grammar: Grammar, stms: LinkedList[Tree]): Unit = {
    var startRule: Option[NonTerminalId] = None

    val startRules = mutable.Set.empty[NonTerminalId]

    /* Collect defs/vals of type Rule[_] */

    def ruleDefinition(tree: Tree, name: TermName, tpt: Tree, value: Tree): Unit = {
      val nonTerminalId = grammar.nonTerminalIdProvider.next()
      grammar.namedNonTerminals += ((name.toString, nonTerminalId))
      grammar.nonTerminalNames  += ((nonTerminalId, name.toString))
      grammar.nonTerminals      += ((nonTerminalId, ParserRule(grammar, value)))

      if (hasAnnotation(tree, StartTpe)) {
        startRules += nonTerminalId
      }
    }

    stms.popSome {
      case tree @ q"""$_ def $name : $tpt = $value""" if isRuleTpe(tpt) =>
        ruleDefinition(tree, name, tpt, value)
      case tree @ q"""$_ val $name : $tpt = $value""" if isRuleTpe(tpt) =>
        ruleDefinition(tree, name, tpt, value)
    }

    if (startRules.isEmpty) {
      c.error(c.enclosingPosition, "No start-rule is defined!")
    } else if (startRules.size > 1) {
      val startRuleNames = startRules.toList.map(grammar.nonTerminalNames).sorted
      c.error(c.enclosingPosition, s"Multiple definitions of start-rule: ${startRuleNames.mkString(", ")}!")
    }

    grammar.startRule = startRules.head

  }

  private def ParserRule(grammar: Grammar, tree: Tree): parser.Rule = {

    def helper(tree: Tree): parser.Rule = null

    helper(tree)
  }

}

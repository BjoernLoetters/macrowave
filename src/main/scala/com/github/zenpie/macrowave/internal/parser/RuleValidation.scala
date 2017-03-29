package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.{Grammar, parser}
import com.github.zenpie.macrowave.internal.ids.NonTerminalId

import scala.collection.mutable

object RuleValidation {

  def validateParserRules(grammar: Grammar): Unit = {
    if (!containsUselessRules(grammar)) {
      containsUnreachableRules(grammar)
    }
  }

  private def containsUnreachableRules(grammar: Grammar): Unit = {
    import grammar._
    val reachableNonTerminals = mutable.Set.empty[NonTerminalId]

    def helper(rule: parser.Rule): Unit = rule match {
      case parser.Epsilon(_) => ()
      case parser.Concatenate(l, r, _) => helper(l); helper(r)
      case parser.Alternate(l, r, _) => helper(l); helper(r)
      case parser.PClosure(r, _) => helper(r)
      case parser.Kleene(r, _) => helper(r)
      case parser.Optional(r, _) => helper(r)
      case parser.Transform(r, _, _) => helper(r)
      case parser.Terminal(_, _) => ()
      case parser.NonTerminal(name, _) =>
        val referencedRuleId = namedNonTerminals(name)
        reachableNonTerminals += referencedRuleId
        val referencedRule = nonTerminals(referencedRuleId)
        helper(referencedRule)
    }

    reachableNonTerminals += startRule
    helper(nonTerminals(startRule))

    for ((nonTerminalId, _) <- nonTerminals) {
      if (!reachableNonTerminals.contains(nonTerminalId)) {
        val nonTerminalPosition = nonTerminalPositions(nonTerminalId)
        val nonTerminalName = nonTerminalNames(nonTerminalId)
        c.error(nonTerminalPosition, s"The rule '$nonTerminalName' isn't reachable!")
      }
    }

  }

  private def containsUselessRules(grammar: Grammar): Boolean = {
    import grammar._

    val generating = mutable.Set.empty[NonTerminalId]

    def isGenerating(rule: parser.Rule): Boolean = rule match {
      case parser.Epsilon(_) => true
      case parser.Concatenate(l, r, _) => isGenerating(l) && isGenerating(r)
      case parser.Alternate(l, r, _) => isGenerating(l) || isGenerating(r)
      case parser.PClosure(r, _) => isGenerating(r)
      case parser.NonTerminal(name, _) =>
        val referencedRuleId = namedNonTerminals(name)
        generating.contains(referencedRuleId)
      case _ => true
    }

    def isUselessRule(nonTerminalId: NonTerminalId): Boolean = {
      val nonTerminal = nonTerminals(nonTerminalId)
      if (isGenerating(nonTerminal)) {
        generating += nonTerminalId
        false
      } else {
        true
      }
    }

    var size = 0
    do {
      size = generating.size
      for ((nonTerminalId, _) <- nonTerminals) {
        isUselessRule(nonTerminalId)
      }
    } while (size != generating.size)

    var uselessRulesExist = false
    for ((nonTerminalId, _) <- nonTerminals) {
      if (!generating.contains(nonTerminalId)) {
        uselessRulesExist = true
        val nonTerminalPosition = nonTerminalPositions(nonTerminalId)
        val nonTerminalName = nonTerminalNames(nonTerminalId)
        val prefix = if (nonTerminalId == startRule) "start-rule" else "rule"
        c.error(nonTerminalPosition, s"The $prefix '$nonTerminalName' is useless!")
      }
    }

    uselessRulesExist
  }

}

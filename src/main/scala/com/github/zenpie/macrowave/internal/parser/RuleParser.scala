package com.github.zenpie.macrowave.internal.parser

import java.util.LinkedList

import com.github.zenpie.macrowave.internal.ids.{ActionId, NonTerminalId, TypeId}
import com.github.zenpie.macrowave.internal._

import scala.collection.mutable
import scala.reflect.macros.whitebox

trait RuleParser extends MacroUtils {
  val c: whitebox.Context
  import c.universe._

  private[internal] def parserRulesFromStatements(grammar: Grammar, stms: LinkedList[Tree]): Unit = {
    var startRule: Option[NonTerminalId] = None

    val startRules  = mutable.Set.empty[NonTerminalId]
    val nonTermTpes = mutable.Map.empty[String, Type]

    /* Collect types of non-terminals */

    stms.forsome {
      case tree @ q"""$_ def $name : $tpt = $value""" if isRuleTpe(tpt) =>
        nonTermTpes += ((name.toString, tpt.tpe))
      case tree @ q"""$_ val $name : $tpt = $value""" if isRuleTpe(tpt) =>
        nonTermTpes += ((name.toString, tpt.tpe))
    }

    /* Collect defs/vals of type Rule[_] */

    def ruleDefinition(tree: Tree, name: TermName, tpt: Tree, value: Tree): Unit = {
      val nonTerminalId = grammar.nonTerminalIdProvider.next()
      grammar.namedNonTerminals += ((name.toString, nonTerminalId))
      grammar.nonTerminalNames  += ((nonTerminalId, name.toString))
      grammar.nonTerminals      += ((nonTerminalId, ParserRule(grammar, nonTermTpes, value)))

      if (hasAnnotation(tree, StartTpe)) {
        startRules += nonTerminalId
      }
    }

    stms.popsome {
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

  private def ParserRule(grammar: Grammar, nonTermTpes: mutable.Map[String, Type], tree: Tree): parser.Rule = {

    def typeOf(tree: Tree): TypeId =
      typeId(tree.tpe)

    def typeId(tpe: Type): TypeId = {
      val tpt    = q"$tpe"
      val typeId = grammar.typeIdProvider.next()
      grammar.types += ((typeId, tpt.asInstanceOf[grammar.Tree]))
      typeId
    }

    def actionId(tree: Tree): ActionId = {
      val actionId = grammar.actionIdProvider.next()
      grammar.actions += ((actionId, tree.asInstanceOf[grammar.Tree]))
      actionId
    }

    def helper(tree: Tree): parser.Rule = tree match {
      case q"($a).~[$aTpt, $bTpt]($b)($ev)" =>
        val tid = typeOf(tree)
        val x   = helper(a)
        val y   = helper(b)
        parser.Concatenate(x, y, tid)
      case q"($a).|[$bTpt]($b)" =>
        val tid = typeOf(tree)
        val x   = helper(a)
        val y   = helper(b)
        parser.Alternate(x, y, tid)
      case q"$prefix.$name[..$_]($r).^^[$resTpt]($action)"
        if isMacrowavePackageObj(prefix) && validRuleActions(name.toString) =>
        val aid = actionId(action)
        val tid = typeOf(tree)
        val x   = helper(r)
        parser.Transform(x, aid, tid)

      case q"($r).*[$rTpt]" =>
        val tid = typeOf(tree)
        val x   = helper(r)
        parser.Kleene(x, tid)
      case q"($r).+[$rTpt]" =>
        val tid = typeOf(tree)
        val x   = helper(r)
        parser.PClosure(x, tid)
      case q"($r).?[$rTpt]" =>
        val tid = typeOf(tree)
        val x   = helper(r)
        parser.Optional(x, tid)

      case q"$_.this.$nonTerminalName" =>
        val ntName = nonTerminalName.toString()
        val ntTpe  = nonTermTpes(ntName)
        parser.NonTerminal(ntName, typeId(ntTpe))
      case q"$prefix.singletonRule($_.this.$tokenName)" if isMacrowavePackageObj(prefix) =>
        parser.Terminal(tokenName.toString, typeId(SingletonRuleTpe))

      case x =>
        c.abort(x.pos, s"'${show(x)}' is not a rule!")
    }

    helper(tree)
  }

}

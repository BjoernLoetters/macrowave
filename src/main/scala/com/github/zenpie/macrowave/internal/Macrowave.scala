package com.github.zenpie.macrowave.internal

import java.util.LinkedList

import com.github.zenpie.macrowave.internal.scanner.FiniteAutomaton
import com.github.zenpie.macrowave.internal.parser.{LRAutomaton, RuleValidation, SetComputation, SymbolString}

import scala.reflect.macros.whitebox

class Macrowave(val c: whitebox.Context) extends AnyRef
  with MacroUtils
  with scanner.RuleParser
  with parser.RuleParser {

  import c.universe._

  def transformGrammars(annottees: Tree*): c.Tree = {
    val typedAnnottees = annottees map (c.typecheck(_, silent = false))

    val grammars = typedAnnottees map {
      case tree @ q"""$mods class $cname(...$ctors) extends $superclasses { ..$stms }""" =>
        val grammar = new Grammar(c)
        val stmList = new LinkedList[Tree]()
        stms.foreach(stm => stmList.add(stm))

        scannerRulesFromStatements(grammar, stmList)
        parserRulesFromStatements(grammar, stmList)
        RuleValidation.validateParserRules(grammar)
        val dfa = FiniteAutomaton.generate(grammar)
        SymbolString.fromGrammar(grammar)
        SetComputation.calculateFirstAndFollowSets(grammar)
        val lr0 = LRAutomaton.generateLR0(grammar)

        q"""$mods class $cname(...$ctors) extends $superclasses {}"""
      case x =>
        c.abort(x.pos, "Element annotated with 'grammar' is no Grammar!")
    }

    q"{..$grammars}"
  }

}

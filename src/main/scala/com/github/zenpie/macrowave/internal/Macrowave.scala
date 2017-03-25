package com.github.zenpie.macrowave.internal

import java.util.LinkedList

import scala.reflect.macros.blackbox

class Macrowave(val c: blackbox.Context) extends AnyRef
  with MacroUtils
  with scanner.RuleParser {

  import c.universe._

  def transformGrammars(annottees: Tree*): c.Tree = {
    val typedAnnottees = annottees map (c.typecheck(_, silent = false))

    val grammars = typedAnnottees map {
      case tree @ q"""$mods class $cname(...$ctors) extends $superclasses { ..$stms }""" =>
        val grammar = new Grammar(c)
        val stmList = new LinkedList[Tree]()
        stms.foreach(stm => stmList.add(stm))

        scannerRulesFromStatements(grammar, stmList)

        q"""$mods class $cname(...$ctors) extends $superclasses {}"""
      case x =>
        c.abort(x.pos, "Element annotated with 'grammar' is no Grammar!")
    }

    q"{..$grammars}"
  }

}

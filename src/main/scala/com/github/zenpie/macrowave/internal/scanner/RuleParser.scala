package com.github.zenpie.macrowave.internal.scanner

import java.util.LinkedList

import com.github.zenpie.macrowave.internal.{Grammar, MacroUtils, scanner}

import scala.collection.mutable
import scala.reflect.macros.blackbox
import scala.util.{Failure, Success}

trait RuleParser extends MacroUtils {
  val c: blackbox.Context
  import c.universe._

  private[internal] def scannerRulesFromStatements(grammar: Grammar, stms: LinkedList[Tree]): Unit = {

    /* Collect defs/vals of type RegExp */
    val regexps = mutable.Map.empty[String, scanner.Rule]

    popSome(stms) {
      case tree @ q"""$_ def $name : $tpt = $value""" if RegExpTpe =:= tpt.tpe =>
        regexps += ((name.toString, ScannerRule(regexps, value)))
      case tree @ q"""$_ val $name : $tpt = $value""" if RegExpTpe =:= tpt.tpe =>
        regexps += ((name.toString, ScannerRule(regexps, value)))
    }

    /* Collect defs/vals of type Token */
    popSome(stms) {
      case tree @ q"""$_ def $name : $tpt = $value""" if TokenTpe =:= tpt.tpe =>
        val terminalId = grammar.terminalIdProvider.next()
        grammar.namedTerminals += ((name.toString, terminalId))
        grammar.terminals      += ((terminalId, Token(regexps, value)))
      case tree @ q"""$_ val $name : $tpt = $value""" if TokenTpe =:= tpt.tpe =>
        val terminalId = grammar.terminalIdProvider.next()
        grammar.namedTerminals += ((name.toString, terminalId))
        grammar.terminals      += ((terminalId, Token(regexps, value)))
    }

  }

  private def ScannerRule(refs: mutable.Map[String, scanner.Rule], tree: Tree): scanner.Rule = {
    def parseRegex(tree: Tree, str: String): scanner.Rule =
      RegExpParser.parse(str) match {
        case Success(x) => x
        case Failure(_) => c.abort(tree.pos, "Invalid or unsupported Regex!")
      }

    def helper(tree: Tree): scanner.Rule = tree match {
      case q"$l ~ $r" =>
        val a = helper(l)
        val b = helper(r)
        scanner.Concatenate(a, b)
      case q"$l | $r" =>
        val a = helper(l)
        val b = helper(r)
        scanner.Alternate(a, b)
      case q"$x.*" =>
        val y = helper(x)
        scanner.Kleene(y)
      case q"$x.?" =>
        val y = helper(x)
        scanner.Optional(y)
      case q"$x.+" =>
        val y = helper(x)
        scanner.Concatenate(y, scanner.Kleene(y))

      case q"$prefix.regex(${str: String})" if isMacrowavePackageObj(prefix) =>
        parseRegex(tree, str)
      case q"$prefix.regex($predef.augmentString(${str: String}).r)" if isMacrowavePackageObj(prefix) && isScalaPredef(predef) =>
        parseRegex(tree, str)
      case q"$prefix.literal(${str: String})" if isMacrowavePackageObj(prefix) =>
        if (str.isEmpty) {
          c.abort(tree.pos, "Literals must not be empty!")
        } else {
          val ranges = str.map(c => scanner.Range(c, c): scanner.Rule)
          ranges.tail.foldLeft(ranges.head)(scanner.Concatenate)
        }
      case q"$_.this.${ref: TermName}" =>
        refs(ref.toString)

      case x =>
        c.abort(x.pos, s"'${show(x)}' is not a regex!")
    }

    helper(tree)
  }

  private def Token(refs: mutable.Map[String, scanner.Rule], tree: Tree): scanner.Rule = tree match {
    case q"$prefix.token($r)" if isMacrowavePackageObj(prefix) =>
      ScannerRule(refs, r)
    case x =>
      c.abort(x.pos, s"'${show(x)}' is not a token!")
  }

}

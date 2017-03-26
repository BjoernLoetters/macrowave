package com.github.zenpie.macrowave.internal

import java.util.LinkedList

import scala.reflect.macros.whitebox

private[internal] trait MacroUtils {
  val c: whitebox.Context
  import c.universe._

  private[internal] val RegExpTpe = typeOf[com.github.zenpie.macrowave.RegExp]
  private[internal] val TokenTpe  = typeOf[com.github.zenpie.macrowave.Token]
  private[internal] val StartTpe  = typeOf[com.github.zenpie.macrowave.start]
  private[internal] val SingletonRuleTpe = typeOf[com.github.zenpie.macrowave.Rule[
    com.github.zenpie.macrowave.::[String, com.github.zenpie.macrowave.HNil]]]

  private[internal] val validRuleActions = ((0 to 22) map (n => s"RuleAction$n")).toSet

  private[internal] def isRuleTpe(tree: Tree): Boolean = tree match {
    case tq"$prefix.Rule[$_]" => isMacrowavePackageObj(prefix)
    case tq"$prefix.Rule1[$_]" => isMacrowavePackageObj(prefix)
    case tq"Rule[$_]" => true
    case tq"Rule1[$_]" => true
    case _ => false
  }

  private[internal] def isMacrowavePackageObj(prefix: Tree): Boolean =
    prefix match {
      case Select(Select(Select(Select(Ident(TermName("com")), TermName("github")), TermName("zenpie")), TermName("macrowave")), TermName("package")) => true
      case Select(Select(Select(Ident(TermName("com")), TermName("github")), TermName("zenpie")), TermName("macrowave")) => true
      case Select(This(TypeName("macrowave")), TermName("package")) => true
      case This(TypeName("macrowave")) => true
      case _ => false
    }

  private[internal] def isScalaPredef(prefix: Tree): Boolean =
    prefix match {
      case Select(This(TypeName("scala")), TermName("Predef")) => true
      case _ => false
    }

  private[internal] def hasAnnotation(tree: Tree, annotation: Type): Boolean =
    tree.symbol.annotations.exists(_.tree.tpe =:= annotation)

}

package com.github.zenpie.macrowave.internal

import scala.reflect.macros.whitebox

private[internal] trait MacroUtils {
  val c: whitebox.Context
  import c.universe._

  private[internal] val RegExpTpe = typeOf[com.github.zenpie.macrowave.RegExp]
  private[internal] val TokenTpe = typeOf[com.github.zenpie.macrowave.Token]
  private[internal] val StartTpe = typeOf[com.github.zenpie.macrowave.start]
  private[internal] val WhiteSpaceTpe = typeOf[com.github.zenpie.macrowave.whiteSpace]
  private[internal] val SingletonRuleTpe = typeOf[com.github.zenpie.macrowave.Rule[
    com.github.zenpie.macrowave.::[String, com.github.zenpie.macrowave.HNil]]]

  private[internal] val RuleTpe = typeOf[com.github.zenpie.macrowave.Rule[_]]
  private[internal] val Rule1Tpe = typeOf[com.github.zenpie.macrowave.Rule1[_]]

  private[internal] val validRuleActions = ((0 to 22) map (n => s"RuleAction$n")).toSet

  private[internal] def isRuleTpe(tree: Tree): Boolean =
    if (!tree.isType) false else {
      val tpe = tree.tpe
      val tpeArgs = tpe.typeArgs

      val apRule = appliedType(RuleTpe, tpeArgs)
      val apRule1 = appliedType(Rule1Tpe, tpeArgs)

      val isRule = tpe <:< apRule
      val isRule1 = tpe <:< apRule1

      isRule || isRule1
    }

  private[internal] def isMacrowavePackageObj(prefix: Tree): Boolean =
    prefix match {
      case Select(Select(Select(Select(Ident(TermName("com")), TermName("github")), TermName("zenpie")), TermName("macrowave")), TermName("package")) => true
      case Select(Select(Select(Ident(TermName("com")), TermName("github")), TermName("zenpie")), TermName("macrowave")) => true
      case Select(Select(This(TypeName("zenpie")), TermName("macrowave")), TermName("package")) => true
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

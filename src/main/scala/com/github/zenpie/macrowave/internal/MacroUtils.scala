package com.github.zenpie.macrowave.internal

import java.util.LinkedList

import scala.reflect.macros.blackbox

private[internal] trait MacroUtils {
  val c: blackbox.Context
  import c.universe._

  private[internal] val RegExpTpe = typeOf[com.github.zenpie.macrowave.RegExp]
  private[internal] val TokenTpe  = typeOf[com.github.zenpie.macrowave.Token]

  private[internal] def isMacrowavePackageObj(prefix: Tree): Boolean =
    Option(prefix) exists {
      case Select(Select(Select(Select(Ident(TermName("com")), TermName("github")), TermName("zenpie")), TermName("macrowave")), TermName("package")) => true
      case Select(This(TypeName("macrowave")), TermName("package")) => true
      case _ => false
    }

  private[internal] def isScalaPredef(prefix: Tree): Boolean =
    Option(prefix) exists {
      case Select(This(TypeName("scala")), TermName("Predef")) => true
      case _ => false
    }

  private[internal] def popSome(stms: LinkedList[Tree])(pf: PartialFunction[Tree, Unit]): Unit = {
    val iter = stms.iterator()
    while (iter.hasNext) iter.next() match {
      case elem if pf isDefinedAt elem =>
        pf(elem)
        iter.remove()
      case _ =>
        ()
    }
  }

}

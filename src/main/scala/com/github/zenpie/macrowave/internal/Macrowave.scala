package com.github.zenpie.macrowave.internal

import scala.reflect.macros.blackbox

class Macrowave(val c: blackbox.Context) {
  import c.universe.Quasiquote
  import c.Tree

  def transformGrammar(annottees: Tree*): c.Tree =
    q"{..$annottees}"

}

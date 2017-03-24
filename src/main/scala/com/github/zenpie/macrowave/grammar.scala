package com.github.zenpie.macrowave

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

@compileTimeOnly("Enable macro-paradise to expand macro annotations!")
class grammar extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro com.github.zenpie.macrowave.internal.Macrowave.transformGrammars

}

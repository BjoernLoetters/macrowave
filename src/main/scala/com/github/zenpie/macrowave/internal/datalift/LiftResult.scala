package com.github.zenpie.macrowave.internal.datalift

/**
  * Result of lifting multiple values.
  */
case class LiftResult[Tree](definitions: Seq[Tree],
                            private val treeRefs: Map[TreeRef[Tree, Any], Tree]) {

  def lookup[T](ref: TreeRef[Tree, T]): Tree =
    treeRefs(ref.asInstanceOf[TreeRef[Tree, Any]])

}

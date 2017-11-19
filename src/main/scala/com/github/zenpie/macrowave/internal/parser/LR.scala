package com.github.zenpie.macrowave.internal.parser

object LR {

  sealed trait Item[F <: Item[F]] extends Product with Serializable { self: F =>
    val position: Int
    val symbolString: SymbolString
    val rule: Rule

    final def advancePosition: F =
      setPosition(position + 1)

    def setPosition(position: Int): F

    def currentSymbol: Option[Symbol] =
      symbolString.data.lift(position)

  }

  case class Item0(position: Int, symbolString: SymbolString, rule: Rule) extends Item[Item0] {

    override def setPosition(position: Int): Item0 =
      copy(position = position + 1)

  }

  case class Item1(position: Int, symbolString: SymbolString, rule: Rule, lookahead: Set[TerminalSymbol]) extends Item[Item1] {

    override def setPosition(position: Int): Item1 =
      copy(position = position + 1)

  }

}

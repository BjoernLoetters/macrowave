package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.Grammar

import scala.collection.mutable

object LR {

  /* items */
  sealed trait Item[F <: Item[F]] extends Product with Serializable { self: F =>
    val position: Int
    val symbolString: SymbolString
    val rule: Rule

    final def advancePosition: F =
      setPosition(position + 1)

    def setPosition(position: Int): F

    def currentSymbol: Option[Symbol] =
      symbolString.data.lift(position)
    
    def isAtEnd: Boolean =
      position >= symbolString.data.length
    
    def skipEpsilons: F = {
      var p = position
      
      while (p < symbolString.data.size && symbolString.data(p) == EpsilonSymbol) {
        p += 1
      }
      
      if (p == position) {
        this
      } else {
        setPosition(p)
      }
    }
    
    def show(implicit grammar: Grammar): String = {
      val prefix = symbolString.data.slice(0, position)
      val suffix = symbolString.data.slice(position, symbolString.data.size)
      val separator = (prefix.size, suffix.size) match {
        case (0, 0) => "."
        case (0, _) => ". "
        case (_, 0) => " ."
        case _ => " . "
      }
  
      prefix.map(_.show(grammar)).mkString(" ") + separator + suffix.map(_.show(grammar)).mkString(" ")
    }
  
    override def toString: String = {
      val prefix = symbolString.data.slice(0, position)
      val suffix = symbolString.data.slice(position, symbolString.data.size)
      val separator = (prefix.size, suffix.size) match {
        case (0, 0) => "."
        case (0, _) => ". "
        case (_, 0) => " ."
        case _ => " . "
      }
        
      prefix.mkString(" ") + separator + suffix.mkString(" ")
    }

  }

  case class Item0(position: Int, symbolString: SymbolString, rule: Rule) extends Item[Item0] {

    override def setPosition(position: Int): Item0 =
      copy(position = position)

  }

  case class Item1(position: Int, symbolString: SymbolString, rule: Rule, lookahead: Set[TerminalSymbol]) extends Item[Item1] {

    override def setPosition(position: Int): Item1 =
      copy(position = position)

  }
  
  /* edges */
  sealed trait Edge[I <: Item[I]] {
    val symbol: Symbol
    val from: State[I]
    val to: State[I]
    
    def show(implicit grammar: Grammar): String =
      from.id + s" --${symbol.show(grammar)}--> " + to.id
    
    override def toString: String =
      from.id + s" --$symbol--> " + to.id
  }
  
  case class Edge0(symbol: Symbol, from: State[Item0], to: State[Item0]) extends Edge[Item0]
  
  case class Edge1(symbol: Symbol, from: State[Item1], to: State[Item1]) extends Edge[Item1]
  
  /* states */
  sealed trait State[I <: Item[I]] {
    val id: Int
    val items: mutable.Set[I]
    val edges: mutable.Set[_ <: Edge[I]]
  
    def show(implicit grammar: Grammar): String =
      s"State $id {\n  ${items.map(_.show(grammar)).mkString("\n  ")}\n}"
    
    override def toString: String =
      s"State $id {\n  ${items.mkString("\n  ")}\n}"
  
    override final def equals(any: Any): Boolean = any match {
      case state: State[I] => state.items == items
      case _ => false
    }
  }
  
  case class State0(id: Int, items: mutable.Set[Item0], edges: mutable.Set[Edge0]) extends State[Item0]
  
  case class State1(id: Int, items: mutable.Set[Item1], edges: mutable.Set[Edge1]) extends State[Item1]
  
  /* automatons */
  sealed trait Automaton[I <: Item[I]] {
    val start: State[I]
    val states: mutable.Set[_ <: State[I]]
    val nonTerminalEdges: mutable.Set[_ <: Edge[I]]
    val terminalEdges: mutable.Set[_ <: Edge[I]]
  
    def show(implicit grammar: Grammar): String = {
      val statesString = states.map(_.show(grammar)).mkString("\n")
      val ntEdgesString = nonTerminalEdges.map(_.show(grammar)).mkString("\n")
      val tEdgesString = terminalEdges.map(_.show(grammar)).mkString("\n")
      "[START STATE] " + start.id + "\n[STATES]\n" + statesString + "\n\n[NON TERMINAL EDGES]\n" + ntEdgesString + "\n\n[TERMINAL EDGES]\n" + tEdgesString
    }
    
    override def toString: String = {
      val statesString = states.mkString("\n")
      val ntEdgesString = nonTerminalEdges.mkString("\n")
      val tEdgesString = terminalEdges.mkString("\n")
      "[START STATE] " + start.id + "\n[STATES]\n" + statesString + "\n\n[NON TERMINAL EDGES]\n" + ntEdgesString + "\n\n[TERMINAL EDGES]\n" + tEdgesString
    }
    
  }

  case class Automaton0(
    start: State0,
    states: mutable.Set[State0],
    nonTerminalEdges: mutable.Set[Edge0],
    terminalEdges: mutable.Set[Edge0]
  ) extends Automaton[Item0]
  
  case class Automaton1(
    start: State1,
    states: mutable.Set[State1],
    nonTerminalEdges: mutable.Set[Edge1],
    terminalEdges: mutable.Set[Edge1]
  ) extends Automaton[Item1]
  
}

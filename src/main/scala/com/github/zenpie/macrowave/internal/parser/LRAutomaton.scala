package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal._
import com.github.zenpie.macrowave.internal.parser.LR.Item0

import scala.collection.mutable

object LRAutomaton {
  
  def generateLR0(grammar: Grammar): LR.Automaton0 = {
    val counter = new Counter(1)
    
    val states = mutable.Set[LR.State0]()
    val ntEdges = mutable.Set[LR.Edge0]()
    val tEdges = mutable.Set[LR.Edge0]()
  
    val startItems = grammar.symbolStrings(grammar.startRule)
      .map(Item0(0, _, grammar.nonTerminals(grammar.startRule)))
    val start = LR.State0(counter.next(), startItems, mutable.Set())
    closure(start, grammar)
    states += start
    
    val worklist = mutable.Queue(start)
    
    while (worklist.nonEmpty) {
      val state = worklist.dequeue()
      states += state
      
      for (item <- state.items) {
        item.currentSymbol.foreach { symbol =>
          val next = follow(state, symbol, grammar, counter)
          val unique = states.find(_ == next).orElse(worklist.find(_ == next)).getOrElse(next)
          val edge = LR.Edge0(symbol, state, unique)
          
          symbol match {
            case _: NonTerminalSymbol => ntEdges += edge
            case _: TerminalSymbol => tEdges += edge
          }
          
          if (!worklist.contains(unique) && !states.contains(unique)) {
            worklist.enqueue(unique)
          }
        }
      }
    }
    
    LR.Automaton0(start, states, ntEdges, tEdges)
  }
  
  private def closure(state: LR.State0, grammar: Grammar): Unit = {
    val worklist = state.items.map(_.skipEpsilons).to[mutable.Queue]
    state.items.clear()
    
    while (worklist.nonEmpty) {
      val item = worklist.dequeue()
      
      state.items += item
      
      item.currentSymbol match {
        case Some(NonTerminalSymbol(id)) =>
          val reachable = grammar.symbolStrings(id)
            .map(Item0(0, _, grammar.nonTerminals(id)))
            .map(_.skipEpsilons)
          
          worklist ++= reachable.filter(item => !worklist.contains(item) && !state.items.contains(item))
        case _ =>
      }
    }
  }
  
  private def follow(state: LR.State0, symbol: Symbol, grammar: Grammar, counter: Counter): LR.State0 = {
    val items = mutable.Set[LR.Item0]()
    val result = LR.State0(counter.next(), items, mutable.Set())
    
    for {
      item <- state.items
      if !item.isAtEnd
      if item.currentSymbol.contains(symbol)
    } items += item.advancePosition
    
    closure(result, grammar)
    
    result
  }
  
}

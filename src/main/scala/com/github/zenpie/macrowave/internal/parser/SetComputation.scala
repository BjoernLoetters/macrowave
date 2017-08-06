package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.Grammar

import scala.collection.mutable

object SetComputation {

  /**
    * @see http://lara.epfl.ch/w/cc09:algorithm_for_first_and_follow_sets
    */
  def calculateFirstAndFollowSets(grammar: Grammar): Unit = {
    var nchanged = false
    var fchanged = false
    var flchanged = false

    def firstSet(symbol: Symbol): mutable.Set[TerminalSymbol] =
      grammar.firstSet.getOrElseUpdate(symbol, mutable.Set.empty[TerminalSymbol])

    def followSet(symbol: Symbol): mutable.Set[TerminalSymbol] =
      grammar.followSet.getOrElseUpdate(symbol, mutable.Set.empty[TerminalSymbol])

    def isNullable(array: Vector[Symbol]): Boolean =
      array.toSet.subsetOf(grammar.nullable.asInstanceOf[mutable.Set[Symbol]])

    for (terminalId <- grammar.terminals.keys) {
      val terminalSymbol = TokenSymbol(terminalId)
      firstSet(terminalSymbol) += terminalSymbol
      followSet(terminalSymbol)
    }
    for (nonTerminalId <- grammar.nonTerminals.keys) {
      val nonTerminalSymbol = NonTerminalSymbol(nonTerminalId)
      firstSet(nonTerminalSymbol)
      followSet(nonTerminalSymbol)
    }
    followSet(NonTerminalSymbol(grammar.startRule)) += EofSymbol

    firstSet(EpsilonSymbol) += EpsilonSymbol
    firstSet(EofSymbol)     += EofSymbol

    do {
      nchanged = false
      fchanged = false
      flchanged = false

      for ((nonTerminalId, rules) <- grammar.symbolStrings) {
        for (rule <- rules) {
          val symbols = rule.data
          val k       = symbols.size
          val X       = NonTerminalSymbol(nonTerminalId)

          if (isNullable(symbols)) {
            val psize = grammar.nullable.size
            grammar.nullable += X
            nchanged ||= psize != grammar.nullable.size
          }

          var i = 0
          while (i < k) {
            if (i == 0 || isNullable(symbols.slice(0, i))) {
              val psize = firstSet(X).size
              firstSet(X) ++= firstSet(symbols(i))
              fchanged ||= psize != firstSet(X).size
            }

            if (i == k - 1) {
              val psize = followSet(symbols(i)).size
              followSet(symbols(i)) ++= followSet(X)
              flchanged ||= psize != followSet(symbols(i)).size
            }

            var j = i + 1
            while (j < k) {
              if (isNullable(symbols.slice(i + 1, k))) {
                val psize = followSet(symbols(i)).size
                followSet(symbols(i)) ++= followSet(X)
                flchanged ||= psize != followSet(symbols(i)).size
              }

              if (i + 1 == j || isNullable(symbols.slice(i + 1, j))) {
                val psize = followSet(symbols(i)).size
                followSet(symbols(i)) ++= firstSet(symbols(j))
                flchanged ||= psize != followSet(symbols(i)).size
              }

              j += 1
            }

            i += 1
          }
        }
      }
    } while (fchanged || nchanged || flchanged)
  }

}

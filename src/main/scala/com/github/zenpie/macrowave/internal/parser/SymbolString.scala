package com.github.zenpie.macrowave.internal.parser

import com.github.zenpie.macrowave.internal.ids.{ActionId, NonTerminalId}
import com.github.zenpie.macrowave.internal.{Grammar, NameProvider}

import scala.collection.mutable

case class SymbolString(data: Vector[Symbol], action: Option[ActionId]) {
  def ++(str: SymbolString): SymbolString = {
    require(action.isEmpty)
    require(str.action.isEmpty)
    SymbolString(data ++ str.data, action)
  }

  def show(implicit grammar: Grammar): String = {
    val symbolsStr = data map {
      case EofSymbol                        => EofSymbol.toString
      case EpsilonSymbol                    => EpsilonSymbol.toString
      case NonTerminalSymbol(nonTerminalId) => grammar.nonTerminalNames(nonTerminalId)
      case TokenSymbol(terminalId)          => grammar.terminalNames(terminalId)
    } mkString " "

    val actionStr = action.fold("")(x => " " + grammar.actions(x).toString)

    symbolsStr + actionStr
  }
}

object SymbolString {

  private def apply(symbols: Symbol*): SymbolString =
    SymbolString(symbols.toVector, None)

  private def singleton(symbol: Symbol): Vector[SymbolString] =
    Vector(SymbolString(symbol))

  def fromGrammar(grammar: Grammar): Unit = {
    implicit val grmmr = grammar
    implicit val names = NameProvider.counting()

    for (nonTerminalId <- grammar.nonTerminals.keys) {
      generateSymbolString(nonTerminalId)
    }
  }

  private def concat(xs: Vector[SymbolString], ys: Vector[SymbolString]): Vector[SymbolString] =
    for (x <- xs; y <- ys) yield x ++ y

  private def generateSymbolString(nonTerminalId: NonTerminalId)
                                  (implicit grammar: Grammar, names: NameProvider): Unit = {

    val topLevelRule = grammar.nonTerminals(nonTerminalId)
    val ruleName     = grammar.nonTerminalNames(nonTerminalId)

    def buildSymbolString(rule_ : Rule, topLevel: Boolean)
                         (implicit grammar: Grammar, names: NameProvider): Vector[SymbolString] = rule_ match {

      case Concatenate(left, right, _) =>
        concat(
          buildSymbolString(left, topLevel),
          buildSymbolString(right, topLevel)
        )

      case Alternate(left, right, _) =>
        buildSymbolString(left, topLevel) ++
        buildSymbolString(right, topLevel)

      case PClosure(rule, _) =>
        /*
         * S -> A +
         * -------------
         * S  -> A S'
         * S' -> A S'
         * S' -> epsilon
         */
        val ruleString = buildSymbolString(rule, topLevel = false)

        val synthName  = names.fresh(ruleName)
        val synthId    = grammar.nonTerminalIdProvider.next()

        val nonTermSym    = singleton(NonTerminalSymbol(synthId))
        val synthString   = concat(ruleString, nonTermSym) // TODO: add synthetic action: x, xs => x :: xs
        val epsilonString = singleton(EpsilonSymbol)       // TODO: add synthetic action: _     => Nil

        registerSymbolStrings(synthId, synthName, synthString ++ epsilonString)

        synthString

      case Kleene(rule, _) =>
        /*
         * S -> A *
         * -------------
         * S   -> S'
         * S' -> A S'
         * S' -> epsilon
         */
        val ruleString = buildSymbolString(rule, topLevel = false)

        val synthName  = names.fresh(ruleName)
        val synthId    = grammar.nonTerminalIdProvider.next()

        val nonTermSym    = singleton(NonTerminalSymbol(synthId))
        val synthString   = concat(ruleString, nonTermSym) // TODO: add synthetic action: x, xs => x :: xs
        val epsilonString = singleton(EpsilonSymbol)       // TODO: add synthetic action: _     => Nil

        registerSymbolStrings(synthId, synthName, synthString ++ epsilonString)

        singleton(NonTerminalSymbol(synthId))

      case Optional(rule, _) =>
        /*
         * S -> A ?
         * ----------------
         * S -> S'
         * S' -> A
         * S' -> epsilon
         */
        val synthName = names.fresh(ruleName)
        val synthId   = grammar.nonTerminalIdProvider.next()

        val alternativeSome = buildSymbolString(rule, topLevel = false) // TODO: add synthetic action: x => Some(x)
        val alternativeNone = singleton(EpsilonSymbol)                  // TODO: add synthetic action: _ => None

        registerSymbolStrings(synthId, synthName, alternativeSome ++ alternativeNone)

        singleton(NonTerminalSymbol(synthId))

      case Transform(rule, action, _) =>
        /*
         * S -> A ^^ { ... } ^^ { ... }
         * -----------------
         * S' -> A ^^ { ... }
         * S -> S' ^^ { ... }
         */
        val mapped = buildSymbolString(rule, topLevel = false).map { symbolString =>
          symbolString.copy(action = Option(action))
        }

        if (topLevel) {
          mapped
        } else {
          var lastSymbolString: SymbolString = null

          for (right <- mapped) {
            val synthName = names.fresh(ruleName)
            val synthId   = grammar.nonTerminalIdProvider.next()
            registerSymbolStrings(synthId, synthName, Vector(right))

            lastSymbolString = SymbolString(NonTerminalSymbol(synthId))
          }

          Vector(lastSymbolString)
        }

      case Terminal(name, _) =>
        val terminalId = grammar.namedTerminals(name)
        singleton(TokenSymbol(terminalId))

      case NonTerminal(name, _) =>
        val nonTerminalId = grammar.namedNonTerminals(name)
        singleton(NonTerminalSymbol(nonTerminalId))

      case Epsilon(_) =>
        singleton(EpsilonSymbol)
    }

    val symbolStrings = buildSymbolString(topLevelRule, topLevel = true)
    grammar.symbolStrings.getOrElseUpdate(nonTerminalId, mutable.Set()) ++= symbolStrings
  }

  private def registerSymbolStrings(synthId: NonTerminalId, synthName: String, symbolStrings: Traversable[SymbolString])
                                   (implicit grammar: Grammar): Unit = {

    grammar.nonTerminalNames(synthId)    = synthName
    grammar.namedNonTerminals(synthName) = synthId
    grammar.symbolStrings.getOrElseUpdate(synthId, mutable.Set()) ++= symbolStrings
  }

}

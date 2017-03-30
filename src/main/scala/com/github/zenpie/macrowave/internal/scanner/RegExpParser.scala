package com.github.zenpie.macrowave.internal.scanner

import java.io.StringReader

import com.github.zenpie.macrowave.internal.ids.ScannerRuleId
import com.github.zenpie.macrowave.internal.{Grammar, scanner}

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

final class RegExpParser(grammar: Grammar) extends RegexParsers with ImplicitConversions {
  override protected val whiteSpace: Regex = "".r
  private val reserved = Seq(
    '?', '+', '*', '\\', '(', ')', '[', ']', '|', '.', '{', '}'
  )
  private val reservedInCharClass = Seq(
    '-', '\\', ']', '['
  )

  private def id(): ScannerRuleId = grammar.scannerRuleIdProvider.next()

  def parse(source: String): Try[Rule] =
    if (source.isEmpty) util.Success(EmptyString(id()))
    else parseAll(alternative, new StringReader(source)) match {
      case Success(result, _) => util.Success(result())
      case NoSuccess(error, rest) => util.Failure(new RuntimeException(
        s"$error\n${rest.pos.longString}"))
    }

  private def alternative: Parser[() => Rule] =
    concatenation ~ rep("(?<!\\\\)\\|".r ~> concatenation) ^^ {
      case left ~ alternatives =>
        () => alternatives.foldLeft(left()) {
          case (acc, ele) => Alternate(id(), acc, ele())
        }
    }

  private def concatenation: Parser[() => Rule] =
    rep1(postOp) ^^ {
      case hd :: tl =>
        () => tl.foldLeft(hd()) {
          case (acc, next) => Concatenate(id(), acc, next())
        }
      case _ => sys.error("internal error: rep1 should not return the empty list")
    }

  private def repeat[T](n: Int)(elem: => T): Seq[T] =
    Seq.fill[T](n)(elem)

  private def postOp: Parser[() => Rule] =
    value ~ rep("(?<!\\\\)(\\?|\\+|\\*|(\\{\\s*([0-9]+|[0-9]+\\s*,|,\\s*[0-9]+|[0-9]+\\s*,\\s*[0-9]+)\\s*(?<!\\\\)\\}))".r) ^^ { case value ~ ops =>
      ops.foldLeft(value: () => Rule) {
        case (acc, "?") => () => Alternate(id(), EmptyString(id()), acc())
        case (acc, "+") => () => Concatenate(id(), acc(), Kleene(id(), acc()))
        case (acc, "*") => () => Kleene(id(), acc())
        case (acc, quantifier) =>
          val cleaned = quantifier.replaceAll("(\\s|\\{|\\})+", "")
          if (cleaned.startsWith(",")) {
            /* 0 to m */
            val max = cleaned.replaceAll(",", "").toInt
            () => repeat(max)(Alternate(id(), EmptyString(id()), acc()))
              .foldLeft(EmptyString(id()): Rule)(Concatenate(id(), _, _))
          } else if (cleaned.endsWith(",")) {
            /* at least n */
            val min = cleaned.replaceAll(",", "").toInt
            () => repeat(min)(acc())
              .foldLeft(Kleene(id(), acc()): Rule)(Concatenate(id(), _, _))
          } else if (cleaned.contains(",")) {
            /* n to m */
            val Array(a, b) = cleaned.split(",").map(_.toInt)
            val (n, m) = (Math.min(a, b), Math.max(a, b))
            () => repeat(n)(acc()).foldLeft(
              repeat(m - n)(Alternate(id(), EmptyString(id()), acc()))
                .foldLeft(EmptyString(id()): Rule)(Concatenate(id(), _, _))
            )(Concatenate(id(), _, _))
          } else {
            /* exactly n */
            () => repeat(cleaned.toInt)(acc())
              .foldLeft(EmptyString(id()): Rule)(Concatenate(id(), _, _))
          }
      }
    }

  private def value: Parser[() => Rule] = (
      charClass
    | character(reserved) ^^ { case (from, to) => () => Range(id(), from, to) }
    | "(?<!\\\\)\\(".r ~> alternative <~ "(?<!\\\\)\\)".r
    | "(?<!\\\\)\\.".r ^^^ (() => Range(id(), Char.MinValue, Char.MaxValue))
  )

  private def charClass: Parser[() => Rule] =
    "(?<!\\\\)\\[\\^?-?".r ~ rep(charComponent) ~ "-?(?<!\\\\)\\]".r ^^ {
      case start ~ components ~ end =>
        val ranges =
          if (start.endsWith("-") || end.startsWith("-")) components :+ ('-', '-')
          else components
        val input =
          if (start.length > 1 && start.charAt(1) == '^') scanner.rangeComplement(ranges)
          else ranges
        if (input.isEmpty) () => EmptyString(id())
        else () => input.map { case (from, to) => Range(id(), from, to) }
          .reduceLeft[Rule](Alternate(id(), _, _))
    }

  private def charComponent: Parser[(Char, Char)] =
    character(reservedInCharClass) ~ opt("-" ~> character(reservedInCharClass)) ^^ {
      case (from, _) ~ Some((_, to)) => (from, to)
      case (from, to) ~ None         => (from, to)
    }

  private def prepareForJavaRegexClass(c: Char): String =
    if (reservedInCharClass.contains(c)) "\\" + c
    else "" + c

  private def character(specialChars: Seq[Char]): Parser[(Char, Char)] =
    s"\\\\.|[^${specialChars.map(prepareForJavaRegexClass).mkString}]".r ^^ { s =>
      if (s.charAt(0) == '\\') (s.charAt(1), s.charAt(1))
      else (s.charAt(0), s.charAt(0))
    }

}

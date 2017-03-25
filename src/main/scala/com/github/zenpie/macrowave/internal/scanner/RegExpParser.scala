package com.github.zenpie.macrowave.internal.scanner

import java.io.StringReader
import java.util.regex.Pattern

import com.github.zenpie.macrowave.internal.scanner

import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object RegExpParser extends RegexParsers with ImplicitConversions {
  override protected val whiteSpace: Regex = "".r
  private val reserved = Seq(
    '?', '+', '*', '\\', '(', ')', '[', ']', '|', '.', '{', '}'
  )
  private val reservedInCharClass = Seq(
    '-', '\\', ']', '['
  )

  def parse(source: String): Try[Rule] =
    if (source.isEmpty) util.Success(EmptyString)
    else parseAll(alternative, new StringReader(source)) match {
      case Success(result, _) => util.Success(result)
      case NoSuccess(error, rest) => util.Failure(new RuntimeException(
        s"$error\n${rest.pos.longString}"))
    }

  private def alternative: Parser[Rule] =
    concatenation ~ rep("(?<!\\\\)\\|".r ~> concatenation) ^^ {
      case left ~ alternatives =>
        alternatives.foldLeft(left)(Alternate)
    }

  private def concatenation: Parser[Rule] =
    rep1(postOp) ^^ { case hd :: tl => tl.foldLeft(hd) {
      case (acc, next) => Concatenate(acc, next)
    } case _ => sys.error("internal error: rep1 should not return the empty list") }

  private def postOp: Parser[Rule] =
    value ~ rep("(?<!\\\\)(\\?|\\+|\\*|(\\{\\s*([0-9]+|[0-9]+\\s*,|,\\s*[0-9]+|[0-9]+\\s*,\\s*[0-9]+)\\s*(?<!\\\\)\\}))".r) ^^ { case value ~ ops =>
      ops.foldLeft(value: Rule) {
        case (acc, "?") => Alternate(EmptyString, acc)
        case (acc, "+") => Concatenate(acc, Kleene(acc))
        case (acc, "*") => Kleene(acc)
        case (acc, quantifier) =>
          val cleaned = quantifier.replaceAll("(\\s|\\{|\\})+", "")
          if (cleaned.startsWith(",")) {
            /* 0 to m */
            val max = cleaned.replaceAll(",", "").toInt
            (0 until max).map(_ => Alternate(EmptyString, acc))
              .foldLeft(EmptyString: Rule)(Concatenate)
          } else if (cleaned.endsWith(",")) {
            /* at least n */
            val min = cleaned.replaceAll(",", "").toInt
            (0 until min).map(_ => acc)
              .foldLeft(Kleene(acc): Rule)(Concatenate)
          } else if (cleaned.contains(",")) {
            /* n to m */
            val Array(a, b) = cleaned.split(",").map(_.toInt)
            val (n, m) = (Math.min(a, b), Math.max(a, b))
            (0 until n).map(_ => acc).foldLeft(
              (n until m).map(_ => Alternate(EmptyString, acc))
                .foldLeft(EmptyString: Rule)(Concatenate)
            )(Concatenate)
          } else {
            /* exactly n */
            (0 until cleaned.toInt).map(_ => acc)
              .foldLeft(EmptyString: Rule)(Concatenate)
          }
      }
    }

  private def value: Parser[Rule] =
    charClass | character(reserved) | "(?<!\\\\)\\(".r ~> alternative <~ "(?<!\\\\)\\)".r |
    "(?<!\\\\)\\.".r ^^^ Range(Char.MinValue, Char.MaxValue)

  private def charClass: Parser[Rule] =
    "(?<!\\\\)\\[\\^?-?".r ~ rep(charComponent) ~ "-?(?<!\\\\)\\]".r ^^ {
      case start ~ components ~ end =>
        val ranges =
          if (start.endsWith("-") || end.startsWith("-")) components :+ Range('-', '-')
          else components
        val input =
          if (start.length > 1 && start.charAt(1) == '^') scanner.rangeComplement(ranges)
          else ranges
        if (input.isEmpty) EmptyString
        else input.reduceLeft(Alternate)
    }

  private def charComponent: Parser[Range] =
    character(reservedInCharClass) ~ opt("-" ~> character(reservedInCharClass)) ^^ {
      case Range(from, _) ~ Some(Range(_, to)) => Range(from, to)
      case character ~ None => character
    }

  private def prepareForJavaRegexClass(c: Char): String =
    if (reservedInCharClass.contains(c)) "\\" + c
    else c + ""

  private def character(specialChars: Seq[Char]): Parser[Range] =
    s"\\\\.|[^${specialChars.map(prepareForJavaRegexClass).mkString}]".r ^^ { s =>
      if (s.charAt(0) == '\\') Range(s.charAt(1), s.charAt(1))
      else Range(s.charAt(0), s.charAt(0))
    }

}

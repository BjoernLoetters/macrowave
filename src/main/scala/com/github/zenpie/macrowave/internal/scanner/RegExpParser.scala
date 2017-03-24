package com.github.zenpie.macrowave.internal.scanner

import java.io.StringReader

import com.github.zenpie.macrowave.internal.scanner

import scala.util.Try
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object RegExpParser extends RegexParsers with ImplicitConversions {
  private val reserved = Seq(
    '?', '+', '*', '\\', '(', ')', '[', ']', '|'
  )
  private val reservedInCharClass = Seq(
    '-', '\\', ']', '['
  )

  def parse(source: String): Try[Rule] = parseAll(alternative, new StringReader(source)) match {
    case Success(result, _)     => util.Success(result)
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
    } }

  private def postOp: Parser[Rule] =
    value ~ rep("(?<!\\\\)(\\?|\\+|\\*)".r) ^^ { case value ~ ops =>
      ops.foldLeft(value) {
        case (acc, "?") => Optional(acc)
        case (acc, "+") => Concatenate(acc, Kleene(acc))
        case (acc, "*") => Kleene(acc)
      }
    }

  private def value: Parser[Rule] =
    character(reserved) | charClass |
    "(?<!\\\\)\\(".r ~> alternative <~ "(?<!\\\\)\\)".r

  private def charClass: Parser[Rule] =
    "(?<!\\\\)\\[\\^".r ~> rep1(charComponent) <~ "(?<!\\\\)\\]".r ^^ { components =>
      val complement = scanner.rangeComplement(components)
      complement.tail.foldLeft(complement.head: Rule)(Alternate)
    } |
    "(?<!\\\\)\\[".r ~> rep1(charComponent) <~ "(?<!\\\\)\\]".r ^^ {
      case hd :: tl => tl.foldLeft(hd: Rule)(Alternate)
    }

  private def charComponent: Parser[Range] =
    character(reservedInCharClass) ~ opt("-" ~> character(reservedInCharClass)) ^^ {
      case Range(from, _) ~ Some(Range(_, to)) => Range(from, to)
      case character ~ None                    => character
    }

  private def prepareForJavaRegexClass(c: Char): String =
    if (reservedInCharClass.contains(c)) "\\" + c
    else c + ""

  private def character(specialChars: Seq[Char]): Parser[Range] =
    s"\\\\.|[^${specialChars.map(prepareForJavaRegexClass).mkString}]".r ^^ {s =>
      if (s.charAt(0) == '\\') Range(s.charAt(1), s.charAt(1))
      else Range(s.charAt(0), s.charAt(0))
    }

}

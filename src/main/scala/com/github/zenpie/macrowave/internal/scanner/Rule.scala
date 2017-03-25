package com.github.zenpie.macrowave.internal.scanner

sealed trait Rule {
  def show: String
}

case class Concatenate(left: Rule, right: Rule) extends Rule {
  def show = (left, right) match {
    case (_: Alternate, _: Alternate) => s"(${left.show})(${right.show})"
    case (_: Alternate, _)            => s"(${left.show})${right.show}"
    case (_, _: Alternate)            => s"${left.show}(${right.show})"
    case _                            => s"${left.show}${right.show}"
  }
}
case class Alternate(left: Rule, right: Rule) extends Rule {
  def show = left.show + "|" + right.show
}
case class Range(from: Char, to: Char) extends Rule {
  def show = if (from == to) from + "" else s"[$from-$to]"
}
case class Kleene(rule: Rule) extends Rule {
  def show = rule match {
    case _: Concatenate | _: Alternate => s"(${rule.show})*"
    case _ => rule.show + "*"
  }
}
case class Optional(rule: Rule) extends Rule {
  def show = rule match {
    case _: Concatenate | _: Alternate => s"(${rule.show})?"
    case _ => rule.show + "?"
  }
}
case object EmptyString extends Rule {
  def show = ""
}

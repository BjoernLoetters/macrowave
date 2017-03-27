package com.github.zenpie.macrowave.internal.scanner

import scala.collection.mutable

sealed trait Rule extends Product with Serializable {

  def show          : String

  var nullable      : Boolean          = false
  val firstPosition : mutable.Set[Int] = mutable.Set()
  val lastPosition  : mutable.Set[Int] = mutable.Set()

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

object Range {

  def apply(from: Char, to: Char): Range =
    new Range(from min to, from max to)

  def apply(char: Char): Range =
    new Range(char, char)

  def unapply(any: Any): Option[(Char, Char)] = any match {
    case r: Range => Some((r.from, r.to))
    case _        => None
  }

}

final class Range private(val from: Char, val to: Char) extends Rule {

  def show = if (from == to) from + "" else s"[$from-$to]"

  override def toString = s"Range($from, $to)"

  override def equals(other: Any): Boolean = other match {
    case Range(f, t) => f == from && t == to
    case _ => false
  }

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Range]

  override def hashCode(): Int =
    31 * Character.hashCode(from) + 31 * Character.hashCode(to)

  override def productElement(n: Int): Any =
    if (n < 0 || n > 1) throw new IndexOutOfBoundsException
    else if (n == 0) from
    else to

  override def productArity: Int = 2

  val positions: Array[Int] = new Array((to - from) + 1)

}

case class Kleene(rule: Rule) extends Rule {

  def show = rule match {
    case _: Concatenate | _: Alternate => s"(${rule.show})*"
    case _ => rule.show + "*"
  }

}

case object EmptyString extends Rule {

  def show = ""

  nullable = true

}

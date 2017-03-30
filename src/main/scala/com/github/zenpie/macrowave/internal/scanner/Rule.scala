package com.github.zenpie.macrowave.internal.scanner

import com.github.zenpie.macrowave.internal.ids.ScannerRuleId

import scala.annotation.switch
import scala.util.hashing.MurmurHash3

sealed trait Rule extends Product with Serializable {

  def show: String
  val id  : ScannerRuleId

}

case class Concatenate(id: ScannerRuleId, left: Rule, right: Rule) extends Rule {

  def show = (left, right) match {
    case (_: Alternate, _: Alternate) => s"(${left.show})(${right.show})"
    case (_: Alternate, _)            => s"(${left.show})${right.show}"
    case (_, _: Alternate)            => s"${left.show}(${right.show})"
    case _                            => s"${left.show}${right.show}"
  }

}

case class Alternate(id: ScannerRuleId, left: Rule, right: Rule) extends Rule {

  def show = left.show + "|" + right.show

}

object Range {

  def apply(id: ScannerRuleId, from: Char, to: Char): Range =
    new Range(id, from min to, from max to)

  def apply(id: ScannerRuleId, char: Char): Range =
    new Range(id, char, char)

  def unapply(any: Any): Option[(ScannerRuleId, Char, Char)] = any match {
    case r: Range => Some((r.id, r.from, r.to))
    case _        => None
  }

}

final class Range private(val id: ScannerRuleId, val from: Char, val to: Char) extends Rule {

  def show = if (from == to) from + "" else s"[$from-$to]"

  def intersects(other: Range): Boolean =
    to >= other.from && from <= other.to

  override def toString =
    if (from == to) s"Range($id,$from)"
    else s"Range($id,$from,$to)"

  override def equals(other: Any): Boolean = other match {
    case Range(i, f, t) => f == from && t == to && i == id
    case _ => false
  }

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Range]

  override def hashCode(): Int =
    MurmurHash3.productHash(this)

  override def productElement(n: Int): Any = (n: @switch) match {
    case 0 => id
    case 1 => from
    case 2 => to
    case _ => throw new IndexOutOfBoundsException
  }

  override def productArity: Int = 3

}

case class Kleene(id: ScannerRuleId, rule: Rule) extends Rule {

  def show = rule match {
    case _: Concatenate | _: Alternate => s"(${rule.show})*"
    case _ => rule.show + "*"
  }

}

case class EmptyString(id: ScannerRuleId) extends Rule {

  def show = ""

}

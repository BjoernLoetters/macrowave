package com.github.zenpie.macrowave.internal

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

package object scanner {

  implicit val rangeOrdering = new Ordering[Range] {
    def compare(a: Range, b: Range): Int = Integer.compare(a.from, b.from)
  }

  def rangeComplement(ranges: Seq[Range]): Seq[Range] = {
    val merged = resolveIntersections(ranges)
    val result = ArrayBuffer[Range]()
    var i = 0
    var s = 0.toChar
    while (i < merged.size) {
      val range = merged(i)
      if (s != range.from) {
        result += Range(s, (range.from - 1).toChar)
      }
      if (range.to < Char.MaxValue) {
        s = (range.to + 1).toChar
      }
      i += 1
    }
    if (s < Char.MaxValue) {
      result += Range(s, Char.MaxValue)
    }
    result
  }

  def rangesIntersect(a: Range, b: Range): Boolean =
    a.to >= b.from && a.from <= b.to

  def resolveIntersections(ranges: Seq[Range]): Seq[Range] = {
    val result = ArrayBuffer(ranges: _*)
    var i, j, intersection, insert = 0
    var a, b: Range = null

    while (i < result.size) {
      j = i + 1
      a = result(i)
      while (j < result.size) {
        b = result(j)
        if (rangesIntersect(a, b)) {
          result.remove(j)
          result.remove(i)
          i -= 1
          intersection = Math.max(0, Math.min(a.to, b.to) - Math.max(a.from, b.from))
          if (a.from <= b.from && a.to >= b.to) {
            // a contains b
            insert = j - 1
            result.insert(insert, Range(b.from, b.to))
            insert += 1
            if ((b.from.toInt - 1) - a.from.toInt >= 0) {
              result.insert(insert, Range(a.from, (b.from - 1).toChar))
              insert += 1
            }
            if (a.to.toInt - (b.to.toInt + 1) >= 0) {
              result.insert(insert, Range((b.to + 1).toChar, a.to))
            }
          } else if (b.from <= a.from && b.to >= a.to) {
            // b contains a
            insert = j - 1
            result.insert(insert, Range(a.from, a.to))
            insert += 1
            if ((a.from.toInt - 1) - b.from.toInt >= 0) {
              result.insert(insert, Range(b.from, (a.from - 1).toChar))
              insert += 1
            }
            if (b.to.toInt - (a.to.toInt + 1) >= 0) {
              result.insert(insert, Range((a.to + 1).toChar, b.to))
            }
          } else if (a.from <= b.from) {
            insert = j - 1
            result.insert(insert, Range(b.from, a.to))
            insert += 1
            if ((b.from.toInt - 1) - a.from.toInt >= 0) {
              result.insert(insert, Range(a.from, (b.from - 1).toChar))
              insert += 1
            }
            if (b.to.toInt - (a.to.toInt + 1) >= 0) {
              result.insert(insert, Range((a.to + 1).toChar, b.to))
            }
          } else {
            insert = j - 1
            result.insert(insert, Range(a.from, b.to))
            insert += 1
            if ((a.from.toInt - 1) - b.from.toInt >= 0) {
              result.insert(insert, Range(b.from, (a.from - 1).toChar))
              insert += 1
            }
            if (a.to.toInt - (b.to.toInt + 1) >= 0) {
              result.insert(insert, Range((b.to + 1).toChar, a.to))
            }
          }
          j = result.size
        }
        j += 1
      }
      i += 1
    }
    val r = result.toArray
    Sorting.quickSort(r)
    r
  }

}

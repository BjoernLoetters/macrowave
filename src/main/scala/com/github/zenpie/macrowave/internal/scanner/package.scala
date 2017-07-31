package com.github.zenpie.macrowave.internal

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

package object scanner {

  implicit val rangeOrdering = new Ordering[(Char, Char)] {
    def compare(a: (Char, Char), b: (Char, Char)): Int =
      Character.compare(a._1, b._2)
  }

  def rangeComplement(ranges: Seq[(Char, Char)]): Seq[(Char, Char)] = {
    val merged = resolveIntersections(ranges)
    val result = ArrayBuffer[(Char, Char)]()
    var i = 0
    var s = 0.toChar
    while (i < merged.size) {
      val (from, to) = merged(i)
      if (s != from) {
        result += ((s, (from - 1).toChar))
      }
      if (to < Char.MaxValue) {
        s = (to + 1).toChar
      }
      i += 1
    }
    if (s < Char.MaxValue) {
      result += ((s, Char.MaxValue))
    }
    result
  }
  
  def rangesIntersect(afrom: Char, ato: Char, bfrom: Char, bto: Char): Boolean = 
    ato >= bfrom && afrom <= bto

  def resolveIntersections(ranges: Seq[(Char, Char)]): Seq[(Char, Char)] = {
    val result = ArrayBuffer(ranges: _*)
    var i, j, intersection, insert = 0
    var afrom, ato, bfrom, bto: Char = '\u0000'

    while (i < result.size) {
      j = i + 1
      val (afrom_, ato_) = result(i)
      afrom = afrom_
      ato = ato_
      while (j < result.size) {
        val (bfrom_, bto_) = result(j)
        bfrom = bfrom_
        bto = bto_
        if (rangesIntersect(afrom, ato, bfrom, bto)) {
          result.remove(j)
          result.remove(i)
          i -= 1
          intersection = Math.max(0, Math.min(ato, bto) - Math.max(afrom, bfrom))
          if (afrom <= bfrom && ato >= bto) {
            // a contains b
            insert = j - 1
            result.insert(insert, (bfrom, bto))
            insert += 1
            if ((bfrom.toInt - 1) - afrom.toInt >= 0) {
              result.insert(insert, (afrom, (bfrom - 1).toChar))
              insert += 1
            }
            if (ato.toInt - (bto.toInt + 1) >= 0) {
              result.insert(insert, ((bto + 1).toChar, ato))
            }
          } else if (bfrom <= afrom && bto >= ato) {
            // b contains a
            insert = j - 1
            result.insert(insert, (afrom, ato))
            insert += 1
            if ((afrom.toInt - 1) - bfrom.toInt >= 0) {
              result.insert(insert, (bfrom, (afrom - 1).toChar))
              insert += 1
            }
            if (bto.toInt - (ato.toInt + 1) >= 0) {
              result.insert(insert, ((ato + 1).toChar, bto))
            }
          } else if (afrom <= bfrom) {
            insert = j - 1
            result.insert(insert, (bfrom, ato))
            insert += 1
            if ((bfrom.toInt - 1) - afrom.toInt >= 0) {
              result.insert(insert, (afrom, (bfrom - 1).toChar))
              insert += 1
            }
            if (bto.toInt - (ato.toInt + 1) >= 0) {
              result.insert(insert, ((ato + 1).toChar, bto))
            }
          } else {
            insert = j - 1
            result.insert(insert, (afrom, bto))
            insert += 1
            if ((afrom.toInt - 1) - bfrom.toInt >= 0) {
              result.insert(insert, (bfrom, (afrom - 1).toChar))
              insert += 1
            }
            if (ato.toInt - (bto.toInt + 1) >= 0) {
              result.insert(insert, ((bto + 1).toChar, ato))
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

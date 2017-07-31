package com.github.zenpie.macrowave.internal.scanner

import java.util

import com.github.zenpie.macrowave.internal.{Grammar, IdProvider, scanner}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

case class FiniteAutomaton(
  table    : Array[Array[Int]],
  start    : Int,
  accepting: java.util.BitSet
)

object FiniteAutomaton {

  def main(args: Array[String]): Unit = {
    RegExpParser.parse("foo|boo") match {
      case Success(result) =>
        generate(Seq(result))
      case Failure(e) =>
        e.printStackTrace()
    }
  }

  def generate(rule: Seq[Rule]): FiniteAutomaton = {
    val complete =
      if (rule.isEmpty) EmptyString
      else rule.reduceLeft(Alternate)
    convert(complete)
  }

  private def collect(rule: Rule, correspondence: mutable.Map[Int, Int], followPosition: mutable.Map[Int, mutable.Set[Int]])(implicit positionCounter: IdProvider[Int]): Unit = rule match {
    case Alternate(l, r) =>
      collect(l, correspondence, followPosition)
      collect(r, correspondence, followPosition)
      rule.nullable = l.nullable || r.nullable
      rule.firstPosition ++= l.firstPosition
      rule.firstPosition ++= r.firstPosition
      rule.lastPosition ++= l.lastPosition
      rule.lastPosition ++= r.lastPosition
    case Concatenate(l, null) =>
      /* end-marker assumed instead of null */
      /* claim next position for the end-marker */
      val end = positionCounter.next()
      correspondence(end) = -1
      collect(l, correspondence, followPosition)
      /* the end-marker is never nullable, therefore this rule is never nullable */
      rule.nullable = false
      rule.firstPosition ++= l.firstPosition
      if (l.nullable) {
        rule.firstPosition += end
      }
      rule.lastPosition += end
      for (i <- l.lastPosition) {
        followPosition.getOrElseUpdate(i, mutable.Set[Int]()) += end
      }

    case Concatenate(l, r) =>
      collect(l, correspondence, followPosition)
      collect(r, correspondence, followPosition)
      rule.nullable = l.nullable && r.nullable
      rule.firstPosition ++= l.firstPosition
      if (l.nullable) {
        rule.firstPosition ++= r.firstPosition
      }
      rule.lastPosition ++= r.lastPosition
      if (r.nullable) {
        rule.lastPosition ++= l.lastPosition
      }
      for (i <- l.lastPosition) {
        followPosition.getOrElseUpdate(i, mutable.Set[Int]()) ++= r.firstPosition
      }
    case range: Range =>
      var i = 0
      while (i < range.positions.length) {
        range.positions(i) = positionCounter.next()
        correspondence(range.positions(i)) = range.from + i
        i += 1
      }
      rule.nullable = false
      rule.firstPosition ++= range.positions
      rule.lastPosition ++= range.positions
    case Kleene(r) =>
      collect(r, correspondence, followPosition)
      rule.nullable = true
      rule.firstPosition ++= r.firstPosition
      rule.lastPosition ++= r.lastPosition
      for (i <- rule.lastPosition) {
        followPosition.getOrElseUpdate(i, mutable.Set[Int]()) ++= rule.firstPosition
      }
    case EmptyString => /* nothing to do here */
  }

  private def calculateDfaByRule(rule: Rule):
    (ArrayBuffer[mutable.Set[Int]], mutable.Set[mutable.Set[Int]], mutable.Map[mutable.Set[Int], mutable.Map[Int, mutable.Set[Int]]]) = {
    val followPosition = mutable.Map[Int, mutable.Set[Int]]()
    val correspondence = mutable.Map[Int, Int]()
    val positionIdProvider = new IdProvider[Int](identity)
    collect(Concatenate(rule, null), correspondence, followPosition)(positionIdProvider)

    val worklist = ArrayBuffer[mutable.Set[Int]](rule.firstPosition)
    val states = ArrayBuffer[mutable.Set[Int]]()
    val finals = mutable.Set[mutable.Set[Int]]()
    val transitions = mutable.Map[mutable.Set[Int], mutable.Map[Int, mutable.Set[Int]]]()
    while (worklist.nonEmpty) {
      val state = worklist.remove(0)
      val tmp = mutable.Map[Int, mutable.Set[Int]]()
      states += state
      for (p <- state) {
        if (correspondence(p) == -1) {
          /* state contains the end-marker */
          finals += state
        }
        tmp.getOrElseUpdate(correspondence(p), mutable.Set()) ++=
          followPosition.getOrElseUpdate(p, mutable.Set())
      }
      for ((c, nstate) <- tmp) {
        if (!(worklist.contains(nstate) || states.contains(nstate))) {
          worklist += nstate
        }
        transitions.getOrElseUpdate(state, mutable.Map())(c) = nstate
      }
    }
    (states, finals, transitions)
  }

  private def convert(rule: Rule): FiniteAutomaton = {
    val (states, finals, transitions) = calculateDfaByRule(rule)

    /* remove the end-marker state */
    states.remove(states.indexOf(mutable.Set.empty))
    transitions.remove(mutable.Set.empty)

    /* minimize automaton */
    val indices = states.zipWithIndex.toMap
    val size = states.size
    val pairs = new java.util.BitSet((((size - 1) * (size - 1)) + (size - 1)) / 2)
    def set(p: Int, q: Int): Unit = if (p != q) {
      pairs.set(Math.max(p, q) + Math.min(p, q) * size)
    }
    def get(p: Int, q: Int): Boolean = p != q && pairs.get(Math.max(p, q) + Math.min(p, q) * size)

    var x, y = 0
    while (x < size) {
      y = 0
      while (y < x) {
        if (finals.contains(states(x)) != finals.contains(states(y))) {
          set(x, y)
        } /* TODO: if states(x) and states(y) have an action => mark them */
        y += 1
      }
      x += 1
    }

    var changed = false
    do {
      changed = false
      x = 0
      while (x < size) {
        y = 0
        while (y < x) {
          if (!get(x, y)) {
            val xt = transitions.getOrElseUpdate(states(x), mutable.Map())
            val yt = transitions.getOrElseUpdate(states(y), mutable.Map())
            if (xt.size != yt.size) {
              changed = true
              set(x, y)
            } else {
              for ((xc, xdst) <- xt) {
                if (yt.contains(xc)) {
                  val yi = indices(yt(xc))
                  val xi = indices(xdst)
                  if (get(xi, yi)) {
                    changed = true
                    set(x, y)
                  }
                } else {
                  changed = true
                  set(x, y)
                }
              }
            }
          }
          y += 1
        }
        x += 1
      }
    } while (changed)

    /*
      start := [q0]
      end   := forall q element of F : [q]
      For each state q, the equivalence class of q consists of all states p for
      which the pair p, q is not marked.
    */

    val equivClassMapping = new Array[mutable.Set[mutable.Set[Int]]](size)
    val equivClasses = ArrayBuffer[mutable.Set[mutable.Set[Int]]]()
    x = 0
    while (x < size) {
      y = 0
      while (y < x) {
        if (!get(x, y)) {
          /* states x and y may be merged together */
          if (equivClassMapping(y) ne null) {
            equivClassMapping(x) = equivClassMapping(y)
            equivClassMapping(y) += states(x)
          }
        }
        y += 1
      }
      if (equivClassMapping(x) eq null) {
        equivClassMapping(x) = mutable.Set(states(x))
        equivClasses += equivClassMapping(x)
      }
      x += 1
    }

    /* construct table */
    val table = Array.fill(equivClasses.size)(Array.fill(Char.MaxValue)(-1))
    var i = 0
    while (i < equivClasses.size) {
      val c = equivClasses(i)
      val ct = transitions(c.head)
      for ((c, dst) <- ct) if (c != -1) {
        table(i)(c) = equivClasses.indexWhere(_.contains(dst))
      }
      i += 1
    }

    val accepting = new util.BitSet(equivClasses.size)
    finals.map(s => equivClasses.indexWhere(_.contains(s)))
      .foreach(accepting.set)

    FiniteAutomaton(table, 0, accepting)
  }

  /** Prints the ascii part (32-126) of the state table */
  private def debug(table: Array[Array[Int]]): Unit = {
    println((32 until 127).map(_.toChar).mkString(" \t", "\t", ""))
    for (state <- table.indices) {
      print(state)
      print("\t")
      for (symbol <- 32 until 127) {
        if (symbol == -1) print("e\t")
        else print(table(state)(symbol) + "\t")
      }
      println()
    }
  }

  /** prints the states, the equivalence table and the transition table */
  private def debug(states: Seq[mutable.Set[Int]], pairs: java.util.BitSet, transitions: mutable.Map[mutable.Set[Int], mutable.Map[Int, mutable.Set[Int]]]): Unit = {
    println("states:")
    println(states.map(_.mkString("{ ", ", ", " }")).zipWithIndex.map(t => t._2 + ": " + t._1).mkString("\n"))
    println("transitions:")
    println(transitions.map { case (key, value) =>
      val from = key.mkString("{ ", ", ", " }")
      from + value.map { case (c, to) =>
        val sym =
          if (c == -1) "<eof>"
          else c.toChar + ""
        s"--$sym--> ${to.mkString("{ ", ", ", " }")}"
      }.mkString("{ ", " | ", " }")
    }.mkString("\n"))
    println("equivalence classes:")
    val size = states.size
    println(" \t" + (0 until size).mkString("\t"))
    for (y <- 0 until size) {
      print(y)
      print("\t")
      for (x <- 0 until size) {
        if (pairs.get(Math.max(x, y) + Math.min(x, y) * size)) {
          print("x\t")
        } else {
          print(" \t")
        }
      }
      println()
    }
  }

}

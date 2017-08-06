package com.github.zenpie.macrowave.internal.scanner

import com.github.zenpie.macrowave.internal.Grammar
import com.github.zenpie.macrowave.internal.ids.{ScannerRuleId, Table}

import scala.collection._
import scala.collection.mutable.ArrayBuffer
import java.util.{ BitSet => JBitSet }

case class FiniteAutomaton(
  table: Array[Array[Int]],
  start: Int,
  finals: JBitSet
)

/**
 * DFA construction based on the algorithm described by Alfred V. Aho, Monica S. Lam,
 * Ravi Sethi and Jeffrey D. Ullman in the "dragon book" (Compilers: Principles,
 * Techniques and Tools (2nd Edition)) on page 179 "Converting a Regular Expression
 * directly to a DFA".
 */
object FiniteAutomaton {
  private final type ActionTable = Table[ScannerRuleId, Action]
  private final type LookupMap = mutable.Map[Int, IntRange]
  private final type FollowMap = mutable.Map[Int, DfaState]
  private final type ScannerTable = mutable.Map[DfaState, mutable.Map[IntRange, DfaState]]
  private final type FinalStateSet = mutable.Set[Int]

  def generate(grammar: Grammar): FiniteAutomaton = {
    implicit val _ = grammar
    implicit val actions = grammar.scannerRuleIdProvider.dataTable[Action](_ => NoAction)

    // each user defined token rule returns the token id as action
    for ((id, rule) <- grammar.terminals) {
      if (!grammar.whiteSpace.contains(id)) {
        actions(rule.id) = TokenAction(id)
      }
    }

    val root = grammar.terminals.values.reduceLeft(Alternate(grammar.scannerRuleIdProvider.next(), _, _))
    val (start, lookup, follow) = collect(root)
    val (states, finals, table) = construct(start, lookup, follow)

    printASCIITable(states, finals, table)

    minimize(states, finals, table)
  }

  private def collect(root: Rule)(implicit actions: ActionTable, grammar: Grammar): (DfaState, LookupMap, FollowMap) = {
    case class DataSet(var nullable: Boolean, firstPos: DfaState, lastPos: DfaState)

    val dataSets = grammar.scannerRuleIdProvider.dataTable[DataSet](_ =>
      DataSet(nullable = false, DfaState.empty, DfaState.empty))

    var position = 0

    val follow = mutable.Map[Int, DfaState]()
    val lookup = mutable.Map[Int, IntRange]()

    def helper(rule: Rule): Unit = rule match {
      case Alternate(id, l, r) =>
        helper(l)
        helper(r)

        val a = dataSets(id)
        val b = dataSets(l.id)
        val c = dataSets(r.id)

        a.nullable = b.nullable || c.nullable
        a.firstPos ++= b.firstPos
        a.firstPos ++= c.firstPos
        a.lastPos  ++= b.lastPos
        a.lastPos  ++= c.lastPos

      case Concatenate(id, l, r) =>
        helper(l)
        helper(r)

        val a = dataSets(id)
        val b = dataSets(l.id)
        val c = dataSets(r.id)

        a.nullable = b.nullable && c.nullable
        a.firstPos ++= b.firstPos
        if (b.nullable) {
          a.firstPos ++= c.firstPos
        }
        a.lastPos ++= c.lastPos
        if (c.nullable) {
          a.lastPos ++= b.lastPos
        }

        for (position <- b.lastPos) {
          follow.getOrElseUpdate(position, DfaState.empty) ++= c.firstPos
          follow(position).updateAction(c.firstPos.action)
        }

      case r: Range =>
        val a = dataSets(r.id)

        a.nullable = false
        a.firstPos += position
        a.lastPos  += position

        lookup(position) = IntRange(r.from, r.to)
        position += 1

      case Kleene(id, r) =>
        helper(r)
        val a = dataSets(id)
        val b = dataSets(r.id)
        a.nullable = true
        a.firstPos ++= b.firstPos
        a.lastPos ++= b.lastPos

        for (i <- a.lastPos) {
          follow.getOrElseUpdate(i, DfaState.empty) ++= a.firstPos
        }

      case EmptyString(id) =>
        val a = dataSets(id)
        a.nullable = true
    }

    // enter an end marker for each token rule
    position = grammar.terminals.size
    (0 until position).foreach(lookup(_) = IntRange(-1, -1))

    helper(root)

    for ((rule, position) <- grammar.terminals.values.zipWithIndex) {
      for (i <- dataSets(rule.id).lastPos) {
        follow.getOrElseUpdate(i, DfaState.empty) += position
        follow(i).updateAction(actions(rule.id))
      }
    }

    // the start state must be a final state iff the whole regex is nullable
    if (dataSets(root.id).nullable) {
      dataSets(root.id).firstPos += position
      lookup(position) = IntRange(-1, -1)
      grammar.terminals.values.foreach { terminalRule =>
        if (dataSets(terminalRule.id).nullable) {
          dataSets(root.id).firstPos.updateAction(actions(terminalRule.id))
        }
      }
    }

    (dataSets(root.id).firstPos, lookup, follow)
  }

  private def construct(start: DfaState, lookup: LookupMap, follow: FollowMap): (Seq[DfaState], FinalStateSet, ScannerTable) = {
    val unmarked = mutable.Queue[DfaState](start)
    val marked   = ArrayBuffer[DfaState]()
    val finals   = mutable.Set[Int]()
    val sigma    = mutable.Map[DfaState, mutable.Map[IntRange, DfaState]]()

    def getDfaState(range: IntRange, edges: mutable.Map[IntRange, DfaState]): DfaState = {
      // there should be at most one range which intersects with `range`
      val ranges = edges.keys
      ranges.find(other => range.from <= other.to && other.from <= range.to) match {
        case Some(other) =>
          // intersection found
          val istart = other.from max range.from
          val iend = other.to min range.to

          val old = edges.remove(other).get

          if (other.from < istart) {
            edges(IntRange(other.from, istart - 1)) = old
          }
          if (other.to > iend) {
            edges(IntRange(iend + 1, other.to)) = old
          }

          val nstate = DfaState.empty
          nstate ++= old
          nstate.updateAction(nstate.action)
          edges(IntRange(range.from, range.to)) = nstate

          nstate
        case None =>
          // no intersection found
          edges.getOrElseUpdate(range, DfaState.empty)
      }
    }

    while (unmarked.nonEmpty) {
      val state = unmarked.dequeue()
      marked += state
      val edges = sigma.getOrElseUpdate(state, mutable.Map())

      for (position <- state) {
        val range = lookup(position)

        if (range.from == -1 && range.to == -1) {
          /* final state */
          finals += (marked.size - 1)
        } else {
          val s = getDfaState(range, edges)
          s ++= follow.getOrElseUpdate(position, DfaState.empty)
          s.updateAction(follow(position).action)
        }
      }

      for ((_, nstate) <- edges) {
        if (!unmarked.contains(nstate) && !marked.contains(nstate)) {
          unmarked.enqueue(nstate)
        }
      }
    }

    (marked, finals, sigma)
  }

  private def minimize(states: Seq[DfaState], finals: FinalStateSet, sigma: ScannerTable)
                      (implicit actions: ActionTable): FiniteAutomaton = {
    val size = states.size
    def gaussianSum(n: Int) = ((n * n) + n) >> 1
    val pairs = new java.util.BitSet(gaussianSum(size - 1))

    def set(p: Int, q: Int): Unit =
      if (p != q) pairs.set(Math.max(p, q) + Math.min(p, q) * size)

    def get(p: Int, q: Int): Boolean =
      p != q && pairs.get(Math.max(p, q) + Math.min(p, q) * size)

    var x, y = 0
    while (x < size) {
      y = 0
      while (y < x) {
        if (finals.contains(x) != finals.contains(y)) {
          set(x, y)
        } else if (states(x).action != NoAction || states(y).action != NoAction) {
          set(x, y)
        }
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
            val xt = sigma.getOrElseUpdate(states(x), mutable.Map())
            val yt = sigma.getOrElseUpdate(states(y), mutable.Map())
            if (xt.size != yt.size) {
              changed = true
              set(x, y)
            } else {
              for ((xc, xdst) <- xt) {
                if (yt.contains(xc)) {
                  val yi = states.indexOf(yt(xc))
                  val xi = states.indexOf(xdst)
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

    val equivClassMapping = new Array[mutable.Set[DfaState]](size)
    val equivClasses = ArrayBuffer[mutable.Set[DfaState]]()
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
    val table = Array.fill(equivClasses.size)(Array.fill(Char.MaxValue + 1)(-1))
    var i = 0
    while (i < equivClasses.size) {
      val ct = sigma(equivClasses(i).head)
      for ((r, dst) <- ct) {
        for (c <- r.from to r.to) {
          table(i)(c) = equivClasses.indexWhere(_.contains(dst))
        }
      }
      i += 1
    }

    val accepting = new java.util.BitSet(equivClasses.size)
    for (i <- equivClasses.indices) {
      if (finals.contains(i)) {
        accepting.set(i)
      }
    }

    FiniteAutomaton(table, 0, accepting)
  }

  private def printASCIITable(states: Seq[DfaState],
                              finals: FinalStateSet,
                              sigma: ScannerTable): Unit = {
    
    println()
    print("  ")
    for (c <- 32 until 127) {
      print(c.toChar + " ")
    }
    println()
    sigma.foreach { case (src, edges) =>
      print(states.indexOf(src) + " ")
      for (c <- 32 until 127) {
        edges.find { case (key, value) => c >= key.from && c <= key.to } match {
          case Some((_, state)) => print(states.indexOf(state) + " ")
          case None => print("  ")
        }
      }
      println()
    }
    println("=" * 10 + " actions " + "=" * 10)
    for ((state, index) <- states.zipWithIndex) {
      val yesno = if (finals.contains(index)) "yes" else "no"
      println(s"state $index (accepting: $yesno): ${state.action}")
    }
    println()
  }

}

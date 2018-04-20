package com.github.zenpie.macrowave

import java.util.LinkedList

package object internal {
  
  private[internal] class Counter(val initial: Int) {
    private var counter = initial
    
    def next(): Int = {
      val result = counter
      counter += 1
      result
    }
    
  }

  private[internal] implicit class LinkedListOps[T](list: LinkedList[T]) {

    def popsome(pf: PartialFunction[T, Unit]): Unit = {
      val iter = list.iterator()
      while (iter.hasNext) iter.next() match {
        case elem if pf isDefinedAt elem =>
          pf(elem)
          iter.remove()
        case _ =>
          ()
      }
    }

    def forsome(pf: PartialFunction[T, Unit]): Unit = {
      val iter = list.iterator()
      while (iter.hasNext) iter.next() match {
        case elem if pf isDefinedAt elem =>
          pf(elem)
        case _ =>
          ()
      }
    }

  }

}

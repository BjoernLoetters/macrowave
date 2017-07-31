package com.github.zenpie.macrowave.internal.scanner

import java.util
import java.util.ConcurrentModificationException

import com.github.zenpie.macrowave.internal.scanner

import scala.collection.mutable.Iterable
import scala.util.hashing.MurmurHash3

object DfaState {

  def apply(values: Int*): DfaState = {
    val result = new DfaState(values.length + 16)
    result ++= values
    result
  }

  def empty: DfaState = new DfaState(16)

}

final class DfaState(capacity: Int) extends Iterable[Int] {
  private var _action: Action = NoAction
  private var data = new Array[Int](capacity)
  private var size_ = 0

  def updateAction(action: Action): Unit = (this._action, action) match {
    case (NoAction, _)                                         => this._action = action
    case (TokenAction(a), TokenAction(b)) if b.value < a.value => this._action = action
    case _                                                     => ()
  }

  def action = _action

  override def size = size_

  def +=(value: Int): DfaState.this.type = {
    var i = util.Arrays.binarySearch(data, 0, size_, value)
    if (i < 0) {
      i += 1
      i = -i
      if (size_ >= data.length) {
        val ncap = Math.ceil(data.length * 1.5f).toInt
        data = util.Arrays.copyOf(data, ncap)
      }
      if (i >= size_) {
        data(size_) = value
      } else {
        System.arraycopy(data, i, data, i + 1, size_ - i)
        data(i) = value
      }
      size_ += 1
    }
    this
  }

  def ++=(values: TraversableOnce[Int]): DfaState.this.type = {
    values.foreach(this.+=)
    this
  }

  def -=(value: Int): DfaState.this.type = {
    val i = util.Arrays.binarySearch(data, 0, size_, value)
    if (i >= 0) {
      System.arraycopy(data, i + 1, data, i, size_ - (i + 1))
      size_ -= 1
    }
    this
  }

  override def forall(f: Int => Boolean): Boolean = {
    var result = true
    val iter = iterator
    while (iter.hasNext && result) {
      result &&= f(iter.next())
    }
    result
  }

  override def hashCode(): Int =
    MurmurHash3.unorderedHash(this)

  override def toString(): String =
    mkString("DfaState(", ",", ")")

  override def equals(other: Any): Boolean = other match {
    case s: DfaState =>
      s.size == this.size &&
      s.forall(this.contains)
    case _ => false
  }

  def contains(value: Int): Boolean =
    util.Arrays.binarySearch(data, 0, size_, value) >= 0

  override def iterator: Iterator[Int] = new Iterator[Int] {
    private var i = 0

    override def hasNext: Boolean = i < DfaState.this.size

    override def next(): Int = {
      if (i >= DfaState.this.size) {
        throw new ConcurrentModificationException()
      }
      val r = data(i)
      i += 1
      r
    }

  }

}

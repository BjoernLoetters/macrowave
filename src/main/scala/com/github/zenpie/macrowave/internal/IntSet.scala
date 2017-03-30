package com.github.zenpie.macrowave.internal

import java.util
import java.util.ConcurrentModificationException

import scala.collection.mutable.Iterable
import scala.util.hashing.MurmurHash3

object IntSet {

  def apply(values: Int*): IntSet = {
    val result = new IntSet(values.length + 16)
    result ++= values
    result
  }

  def empty: IntSet = new IntSet(16)

}

final class IntSet(capacity: Int) extends Iterable[Int] {
  private var data = new Array[Int](capacity)
  private var size_ = 0

  override def size = size_

  def +=(value: Int): IntSet.this.type = {
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

  def ++=(values: TraversableOnce[Int]): IntSet.this.type = {
    values.foreach(this.+=)
    this
  }

  def -=(value: Int): IntSet.this.type = {
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
    mkString("IntSet(", ",", ")")

  override def equals(other: Any): Boolean = other match {
    case s: IntSet =>
      s.size == this.size &&
      s.forall(this.contains)
    case _ => false
  }

  def contains(value: Int): Boolean =
    util.Arrays.binarySearch(data, 0, size_, value) >= 0

  override def iterator: Iterator[Int] = new Iterator[Int] {
    private var i = 0

    override def hasNext: Boolean = i < IntSet.this.size

    override def next(): Int = {
      if (i >= IntSet.this.size) {
        throw new ConcurrentModificationException()
      }
      val r = data(i)
      i += 1
      r
    }

  }

}

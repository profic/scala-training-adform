package com.adform.task

import java.lang.Double.NEGATIVE_INFINITY

import IntervalTree._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Interval(val begin: Double, val end: Double) {

  override def toString = s"Interval($begin, $end)"
}

object IntervalTree {

  implicit val intervalOrd: Ordering[Interval] = new Ordering[Interval] {
    override def compare(o1: Interval, o2: Interval): Int = o1.begin.compareTo(o2.begin)
  }

  def main(args: Array[String]) {
    val it = new IntervalTree(List[Interval](new Interval(1, 10), new Interval(5, 9), new Interval(6, 9), new Interval(7, 100)))

    val containingIntervals: List[Interval] = it.findContainingIntervals(6)

    println(containingIntervals)
  }

  def apply[I <: Interval](intervals: List[I]) = new IntervalTree(intervals)

}

class IntervalTree[I <: Interval](_intervals: List[I]) {

  private val intervals = _intervals.sorted
  private val maxEnd: Array[Double] = new Array[Double](intervals.length)

  initializeMaxEnd(0, intervals.length)

  private def initializeMaxEnd(a: Int, b: Int): Double = {
    if (a >= b) {
      return NEGATIVE_INFINITY
    }
    val m: Int = (a + b) >>> 1
    maxEnd(m) = initializeMaxEnd(a, m)
    math.max(math.max(maxEnd(m), intervals(m).end), initializeMaxEnd(m + 1, b))
  }

  private def findContainingIntervals(x: Double, a: Int, b: Int): mutable.ArrayBuffer[I] = {
    def helper(_x: Double, _a: Int, _b: Int, result: mutable.ArrayBuffer[I]): mutable.ArrayBuffer[I] = {
      if (_a < _b) {
        val m: Int = (_a + _b) >>> 1
        val i: I = intervals(m)
        if (_x < i.begin) {
          helper(_x, _a, m, result)
        } else {
          if (_x <= i.end) {
            result += i
          }
          if (maxEnd(m) >= _x) {
            helper(_x, _a, m, result)
          }
          helper(_x, m + 1, _b, result)
        }
      } else {
        result
      }
    }
    val acc: ArrayBuffer[I] = mutable.ArrayBuffer[I]()
    helper(x, a, b, acc)
  }

  //  private def findContainingIntervals(x: Double, a: Int, b: Int): mutable.ArrayBuffer[Interval] = {
  //    def helper(_x: Double, _a: Int, _b: Int, result: mutable.ArrayBuffer[Interval]) {
  //      if (_a >= _b) {
  //        return
  //      }
  //      val m: Int = (_a + _b) >>> 1
  //      val i: Interval = intervals(m)
  //      if (_x < i.begin) {
  //        helper(_x, _a, m, result)
  //      }
  //      else {
  //        if (_x <= i.end) {
  //          result += i
  //        }
  //        if (maxEnd(m) >= _x) {
  //          helper(_x, _a, m, result)
  //        }
  //        helper(_x, m + 1, _b, result)
  //      }
  //    }
  //    val acc: ArrayBuffer[Interval] = mutable.ArrayBuffer[Interval]()
  //    helper(x, a, b, acc)
  //    acc
  //  }

  def findContainingIntervals(x: Double): List[I] = {
    findContainingIntervals(x, 0, intervals.length).toList
  }

}
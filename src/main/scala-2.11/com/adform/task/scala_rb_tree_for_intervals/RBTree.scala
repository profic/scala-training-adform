package com.adform.task.scala_rb_tree_for_intervals

import java.util

import scala.language.implicitConversions

import collection.JavaConversions._

abstract sealed class Color

case object R extends Color

case object B extends Color

/**
  * A Red-Black Tree.
  */

abstract sealed class Tree[A, Key <% Ordered[Key]] {

  type T = Tree[A, Key]

  type I = Interval[A, Key]

  var cnt = 0

  def min: Key

  def max: Key

  def color: Color

  def value: I

  def left: T

  def right: T

  def isEmpty: Boolean

  def search(key: Key): util.ArrayList[A] = {

    val ord = implicitly[Ordering[Key]]

    def overlaps(a: Key, b: Key): Boolean = {
      ord.compare(a, key) <= 0 && ord.compare(b, key) >= 0
    }

    val as: util.ArrayList[A] = new util.ArrayList[A]()

    def loop(tree: Tree[A, Key]): Unit = {
      cnt += 1

      if (tree == Leaf) return
      else {
        if (tree.left != Leaf && overlaps(tree.left.min, tree.left.max)) loop(tree.left)
        if (overlaps(tree.value.begin, tree.value.end)) {
          as.add(tree.value.data)
        }
        if (tree.right != Leaf && overlaps(tree.right.min, tree.right.max)) loop(tree.right)

      }
    }
    loop(this)


//    def loop(tree: Tree[A, Key], acc: List[A]): List[A] = {
//      cnt += 1
//
//      if (tree == Leaf) acc
//      else {
//        val rightRes = if (tree.left != Leaf && overlaps(tree.left.min, tree.left.max)) loop(tree.left, acc) else acc
//        val theesRes = if (overlaps(tree.value.begin, tree.value.end)) tree.value.data :: acc else acc
//        val leftRes = if (tree.right != Leaf && overlaps(tree.right.min, tree.right.max)) loop(tree.right, acc) else acc
//        rightRes ::: theesRes ::: leftRes
//      }
//    }
//    loop(this, List())
    as
  }


  def add(elem: I): T = {

    def balancedAdd(t: Tree[A, Key]): Tree[A, Key] =
      if (t.isEmpty) T(R, elem, t, t)
      else if (elem < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (elem > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def rotate(z: I, y: I, x: I, a: T, b: T, c: T, d: T): T = {
      T(R, y, T(B, x, a, b), T(B, z, c, d))
    }
    def balanceLeft(c: Color, z: I, l: T, r: T) = (c, l, r) match {
      case (B, T(R, y, T(R, x, a, b), c), d) => rotate(z, y, x, a, b, c, d)
      case (B, T(R, x, a, T(R, y, b, c)), d) => rotate(z, y, x, a, b, c, d)
      case _                                 => T(c, z, l, r)
    }

    def balanceRight(c: Color, x: I, l: T, r: T) = (c, l, r) match {
      case (B, a, T(R, y, b, T(R, z, c, d))) => rotate(z, y, x, a, b, c, d)
      case (B, a, T(R, z, T(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
      case _                                 => T(c, x, l, r)
    }

    def blacken(t: T): T = T(B, t.value, t.left, t.right)

    blacken(balancedAdd(this))
  }

  def fail(m: String) = throw new NoSuchElementException(m)
}

case class Interval[A, I](begin: I, end: I, data: A)
                         (implicit ev1: I ⇒ Ordered[I]) extends Ordered[Interval[A, I]] {

  override def compare(that: Interval[A, I]): Int = {
    val res: Int = begin compare that.begin
    if (res != 0) res else end compare that.end
  }
}

case class T[A, Key](color: Color,
                     value: Interval[A, Key],
                     left: Tree[A, Key],
                     right: Tree[A, Key])(implicit ev: Key ⇒ Ordered[Key]) extends Tree[A, Key] {
  def isEmpty = false

  val ord: Ordering[Key] = implicitly[Ordering[Key]]

  val _min: Key =
    if (left == Leaf) value.begin
    else left.min

  val _max: Key = {
    val leftMax: Key = if (left != Leaf) ord.max(left.max, value.end) else value.end
    val rightMax: Key = if (right != Leaf) ord.max(right.max, value.end) else value.end
    ord.max(leftMax, rightMax)
  }
  //  val _max: I = (left != Leaf, right != Leaf) match {
  //    case (true, true)  ⇒ ord.max(ord.max(left.max, value.end), ord.max(right.max, value.end))
  //    case (true, false) ⇒ ord.max(left.max, value.end)
  //    case (false, true) ⇒ ord.max(right.max, value.end)
  //    case _             ⇒ value.end
  //  }


  override def min: Key = _min

  override def max: Key = _max
}

case object Leaf extends Tree[Nothing, Nothing] {
  def color: Color = B

  def value = fail("An empty tree.")

  def left = fail("An empty tree.")

  def right = fail("An empty tree.")

  def isEmpty = true

  override def min = fail("An empty tree.")

  override def max = fail("An empty tree.")
}

object Tree {

  def main(args: Array[String]) {

    val tree12 = Tree(
      Interval(1, 4, "1"),
      Interval(4, 11, "2"),
      Interval(5, 10, "3"),
      Interval(3, 9, "4"),
      Interval(2, 5, "5"),
      Interval(0, 12, "6")
    )

    println(tree12.search(6))
  }

  def unapply[A, I](t: T[A, I]): Option[(Color, Interval[A, I], Tree[A, I], Tree[A, I])] = {
    Some(t.color, t.value, t.left, t.right)
  }


  def empty[A, Key](implicit ev: Key ⇒ Ordered[Key]): Tree[A, Key] =
    Leaf.asInstanceOf[Tree[A, Key]]

  def apply[A, Key](xs: Interval[A, Key]*)(implicit ev: Key ⇒ Ordered[Key]): Tree[A, Key] = {
    var r = empty[A, Key]
    for (x <- xs) r = r.add(x)
    r
  }
}
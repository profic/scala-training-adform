package com.adform.task.scala_rb_tree_for_intervals

import scala.language.implicitConversions

abstract sealed class Color

case object R extends Color

case object B extends Color

/**
  * An Okasaki's immutable Red-Black Tree implementation modified for range search
  *
  * http://www.ccs.neu.edu/course/cs3500wc/jfp99redblack.pdf
  *
  */

abstract sealed class Tree[A, Key](implicit ev$1: Key => Ordered[Key]) {

  type T = Tree[A, Key]

  type In = Interval[A, Key]

  def min: Key

  def max: Key

  def color: Color

  def value: In

  def left: T

  def right: T

  def isEmpty: Boolean

  def notEmpty: Boolean = !isEmpty

  def search(f: (Key, Key) ⇒ Boolean): List[A] = {

    val ord = implicitly[Ordering[Key]]

    def loop(acc: List[A], rest: List[Tree[A, Key]]): List[A] = {

      rest match {
        case Nil        => acc
        case tree :: ts =>

          // ugly if-else because can't match on Leaf (invariant tree)
          val _ts = if (tree.left.notEmpty && f(tree.left.min, tree.left.max)) tree.left :: ts else ts
          val _ts2 = if (tree.right.notEmpty && f(tree.right.min, tree.right.max)) tree.right :: _ts else _ts
          val theesRes = if (f(tree.value.begin, tree.value.end)) tree.value.data :: acc else acc
          loop(theesRes, _ts2)
      }
    }
    loop(List(), List(this))
  }

  def add(elem: In): T = {

    def balancedAdd(t: T): T =
      if (t.isEmpty) T(R, elem, t, t)
      else if (elem < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (elem > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def rotate(z: In, y: In, x: In, a: T, b: T, c: T, d: T): T = {
      T(R, y, T(B, x, a, b), T(B, z, c, d))
    }
    def balanceLeft(c: Color, z: In, l: T, r: T) = (c, l, r) match {
      case (B, T(R, y, T(R, x, a, b), c), d) => rotate(z, y, x, a, b, c, d)
      case (B, T(R, x, a, T(R, y, b, c)), d) => rotate(z, y, x, a, b, c, d)
      case _                                 => T(c, z, l, r)
    }

    def balanceRight(c: Color, x: In, l: T, r: T) = (c, l, r) match {
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
    if (left.isEmpty) value.begin
    else left.min

  val _max: Key = {
    val leftMax: Key = if (left.notEmpty) ord.max(left.max, value.end) else value.end
    val rightMax: Key = if (right.notEmpty) ord.max(right.max, value.end) else value.end
    ord.max(leftMax, rightMax)
  }

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

  def empty[A, Key](implicit ev: Key ⇒ Ordered[Key]): Tree[A, Key] =
    Leaf.asInstanceOf[Tree[A, Key]] // since Tree is invariant

  def apply[A, Key](xs: Interval[A, Key]*)(implicit ev: Key ⇒ Ordered[Key]): Tree[A, Key] = {
    var r = empty[A, Key]
    for (x <- xs) r = r.add(x)
    r
  }
}
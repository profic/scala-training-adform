
package com.adform.task.scala_rb_tree

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */

abstract sealed class Color

case object R extends Color

case object B extends Color

/**
  * A Red-Black Tree.
  */

abstract sealed class Tree[+A](implicit ev1: A ⇒ Ordered[A]) {

  /**
    * The color of this tree.
    */
  def color: Color

  /**
    * The value of this tree.
    */
  def value: A

  /**
    * The left child of this tree.
    */
  def left: Tree[A]

  /**
    * The right child of this tree.
    */
  def right: Tree[A]

  /**
    * Checks whether this tree is empty or not.
    */
  def isEmpty: Boolean

  /**
    * Adds given element 'x' into this tree.
    *
    * Exercise 3.10a @ PFDS.
    *
    * Time - O(log n)
    * Space - O(log n)
    */

  def add[U >: A](elem: U)(implicit ev1: U ⇒ Ordered[U]): Tree[U] = {

    type T = Tree[U]

    def balancedAdd(t: Tree[A]): Tree[U] = //balancedAdd - just adds element to the left or right (like in any binary tree)
      if (t.isEmpty) T(R, elem)
      else if (elem < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right) // balanceLeft or balanceRight - balances tree
      else if (elem > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def rotate(z: U, y: U, x: U, a: T, b: T, c: T, d: T): T = {
      T(R, y, T(B, x, a, b), T(B, z, c, d))
    }
    def balanceLeft(c: Color, z: U, l: T, r: T) = (c, l, r) match {
      case (B, T(R, y, T(R, x, a, b), c), d) => rotate(z, y, x, a, b, c, d)
      case (B, T(R, x, a, T(R, y, b, c)), d) => rotate(z, y, x, a, b, c, d)
      case _                                 => T(c, z, l, r)
    }

    def balanceRight(c: Color, x: U, l: T, r: T) = (c, l, r) match {
      case (B, a, T(R, y, b, T(R, z, c, d))) => rotate(z, y, x, a, b, c, d)
      case (B, a, T(R, z, T(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
      case _                                 => T(c, x, l, r)
    }

    def blacken(t: T): T = T(B, t.value, t.left, t.right)

    blacken(balancedAdd(this))
  }

  def height: Int =
    if (isEmpty) 0
    else math.max(left.height, right.height) + 1

  /**
    * Fails with given message.
    */
  def fail(m: String) = throw new NoSuchElementException(m)
}

abstract class IntervalTree[A, I] extends Tree {
  /**
    * Adds given element 'x' into this tree.
    *
    * Exercise 3.10a @ PFDS.
    *
    * Time - O(log n)
    * Space - O(log n)
    */


}

case class Interval[A, I](begin: I, end: I, data: A)
                         (implicit ev1: I ⇒ Ordered[I]) extends Ordered[Interval[A, I]] {

  override def compare(that: Interval[A, I]): Int = {
    val res: Int = begin compare that.begin
    if (res != 0) res else end compare that.end
  }
}

case class T[A <% Ordered[A]](color: Color,
                              value: A,
                              left: Tree[A] = Leaf,
                              right: Tree[A] = Leaf) extends Tree[A] {
  def isEmpty = false
}

case object Leaf extends Tree[Nothing] {
  def color: Color = B

  def value: Nothing = fail("An empty tree.")

  def left: Tree[Nothing] = fail("An empty tree.")

  def right: Tree[Nothing] = fail("An empty tree.")

  def isEmpty = true
}

object Tree {

  def main(args: Array[String]) {
  }

  /**
    * Returns an empty red-black tree instance.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def empty[A]: Tree[A] = Leaf


  /**
    * Creates a new red-black tree from given 'xs' sequence.
    *
    * Time - O(n log n)
    * Space - O(log n)
    */
  def apply[A <% Ordered[A]](xs: A*): Tree[A] = {
    var r: Tree[A] = Leaf
    for (x <- xs) r = r.add(x)
    r
  }
}
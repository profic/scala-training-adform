package com.adform.task.scala_rb_tree

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */


sealed trait Color

private case object R extends Color

private case object B extends Color

abstract /*sealed*/ class RBTree[E <% Ordered[E]] {

  protected def color: Color

  def value: E

  def left: RBTree[E]

  def right: RBTree[E]

  def isEmpty: Boolean

  import RBTree._

  protected def make(color: Color, elem: E,
                     left: RBTree[E] = empty[E],
                     right: RBTree[E] = empty[E]): RBTree[E] = {
    RBNode(color, elem, left, right)
  }

  def add(elem: E): RBTree[E] = {

    def balancedAdd(tree: RBTree[E]): RBTree[E] =
      if (tree.isEmpty) make(R, elem)
      else if (elem < tree.value) balanceLeft(tree.color, tree.value, balancedAdd(tree.left), tree.right)
      else if (elem > tree.value) balanceRight(tree.color, tree.value, tree.left, balancedAdd(tree.right))
      else tree

    def balanceLeft(color: Color, elem: E, left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
      case (B, RBNode(R, y, RBNode(R, z, a, b), c), d) =>
        make(R, y, make(B, z, a, b), make(B, elem, c, d))
      case (B, RBNode(R, z, a, RBNode(R, y, b, c)), d) =>
        make(R, y, make(B, z, a, b), make(B, elem, c, d))
      case _ => make(color, elem, left, right)
    }

    def balanceRight(color: Color, elem: E, left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
      case (B, a, RBNode(R, y, b, RBNode(R, _elem, _color, d))) =>
        make(R, y, make(B, elem, a, b), make(B, _elem, _color, d))
      case (B, a, RBNode(R, z, RBNode(R, y, b, _color), d)) =>
        make(R, y, make(B, elem, a, b), make(B, z, _color, d))
      case _ => make(color, elem, left, right)
    }

    def blacken(t: RBTree[E]) = if (t.color == B) t else make(B, t.value, t.left, t.right)

    val balanced: RBTree[E] = balancedAdd(this)
    val _blacken: RBTree[E] = blacken(balanced)
    _blacken
  }

  def height: Int =
    if (isEmpty) 0
    else math.max(left.height, right.height) + 1

  /**
    * Fails with given message.
    */
  def fail(m: String) = throw new NoSuchElementException(m)
}

case class RBNode[E](color: Color,
                                   value: E,
                                   left: RBTree[E],
                                   right: RBTree[E])(implicit ev$1: E => Ordered[E]) extends RBTree[E] {
  def isEmpty = false
}

case object Leaf extends RBTree[Nothing] {
  def color: Color = B

  def value: Nothing = fail("An empty tree.")

  def left: RBTree[Nothing] = fail("An empty tree.")

  def right: RBTree[Nothing] = fail("An empty tree.")

  def isEmpty = true
}

object RBTree {

  /**
    * Creates a new red-black tree from given 'xs' sequence.
    *
    * Time - O(n log n)
    * Space - O(log n)
    */
  def apply[A <% Ordered[A]](xs: A*): RBTree[A] = {
    var r: RBTree[A] = empty[A]
    for (x <- xs) r = r.add(x)
    r
  }

  def empty[A](implicit ordering: Ordering[A]): RBTree[A] = {
    Leaf.asInstanceOf[RBTree[A]]
  }
}

/** *******************************************************************************************************************************************************************************************************************/

//sealed trait Color
//
//private case object R extends Color
//
//private case object B extends Color
//
//abstract /*sealed*/ class RBTree[+E <% Ordered[E]] {
//
//  protected def color: Color
//
//  def value: E
//
//  def left: RBTree[E]
//
//  def right: RBTree[E]
//
//  def isEmpty: Boolean
//
//  protected def make[U >: E <% Ordered[U]](color: Color, elem: U,
//                                           left: RBTree[U] = Leaf,
//                                           right: RBTree[U] = Leaf): RBTree[U] = {
//    RBNode(color, elem, left, right)
//  }
//
//  def add[U >: E <% Ordered[U]](elem: U): RBTree[U] = {
//
//    def balancedAdd(tree: RBTree[E]): RBTree[U] =
//      if (tree.isEmpty) make(R, elem, tree, Leaf)
//      else if (elem < tree.value) balanceLeft(tree.color, tree.value, balancedAdd(tree.left), tree.right)
//      else if (elem > tree.value) balanceRight(tree.color, tree.value, tree.left, balancedAdd(tree.right))
//      else tree
//
//    def balanceLeft(color: Color, elem: E, left: RBTree[U], right: RBTree[E]) = (color, left, right) match {
//      case (B, RBNode(R, y, RBNode(R, z, a, b), c), d) =>
//        make(R, y, make(B, z, a, b), make(B, elem, c, d))
//      case (B, RBNode(R, z, a, RBNode(R, y, b, c)), d) =>
//        make(R, y, make(B, z, a, b), make(B, elem, c, d))
//      case _ => make(color, elem, left, right)
//    }
//
//    def balanceRight(color: Color, elem: E, left: RBTree[E], right: RBTree[U]) = (color, left, right) match {
//      case (B, a, RBNode(R, y, b, RBNode(R, _elem, _color, d))) =>
//        make(R, y, make(B, elem, a, b), make(B, _elem, _color, d))
//      case (B, a, RBNode(R, z, RBNode(R, y, b, _color), d)) =>
//        make(R, y, make(B, elem, a, b), make(B, z, _color, d))
//      case _ => make(color, elem, left, right)
//    }
//
//    def blacken(t: RBTree[U]) = if (t.color == B) t else make(B, t.value, t.left, t.right)
//
//    val balanced: RBTree[U] = balancedAdd(this)
//    val _blacken: RBTree[U] = blacken(balanced)
//    _blacken
//  }
//
//  def height: Int =
//    if (isEmpty) 0
//    else math.max(left.height, right.height) + 1
//
//  /**
//    * Fails with given message.
//    */
//  def fail(m: String) = throw new NoSuchElementException(m)
//}
//
//case class RBNode[E <% Ordered[E]](color: Color,
//                                   value: E,
//                                   left: RBTree[E],
//                                   right: RBTree[E]) extends RBTree[E] {
//  def isEmpty = false
//}
//
//case object Leaf extends RBTree[Nothing] {
//  def color: Color = B
//
//  def value: Nothing = fail("An empty tree.")
//
//  def left: RBTree[Nothing] = fail("An empty tree.")
//
//  def right: RBTree[Nothing] = fail("An empty tree.")
//
//  def isEmpty = true
//}
//
//object RBTree {
//
//  def empty[A]: RBTree[A] = Leaf
//
//  /**
//    * Creates a new red-black tree from given 'xs' sequence.
//    *
//    * Time - O(n log n)
//    * Space - O(log n)
//    */
//  def apply[A <% Ordered[A]](xs: A*): RBTree[A] = {
//    var r: RBTree[A] = Leaf
//    for (x <- xs) r = r.add(x)
//    r
//  }
//}
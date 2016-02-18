package com.adform.task.scala_rb_tree

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */


import RBTree._

abstract sealed class RBTree[E] {

  def lookupOverlapping(key: Long): List[E] = ???

  protected def color: Color

  def value: Interval[E]

  def left: RBTree[E]

  def right: RBTree[E]

  def isEmpty: Boolean

  protected def make(color: Color, elem: Interval[E],
                     left: RBTree[E] = empty,
                     right: RBTree[E] = empty): RBTree[E] = {
    RBTreeImpl(color, elem, left, right)
  }

  def add(elem: Interval[E]): RBTree[E] = {

    def balancedAdd(tree: RBTree[E]): RBTree[E] =
      if (tree.isEmpty) make(R, elem)
      else {
        if (elem < tree.value) balanceLeft(tree.color, tree.value, balancedAdd(tree.left), tree.right)
        else if (elem > tree.value) balanceRight(tree.color, tree.value, tree.left, balancedAdd(tree.right))
        else tree
      }

    def balanceLeft(color: Color, elem: Interval[E], left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
      case (B, RBTreeImpl(R, y, RBTreeImpl(R, z, a, b), c), d) =>
        make(R, y, make(B, z, a, b), make(B, elem, c, d))
      case (B, RBTreeImpl(R, z, a, RBTreeImpl(R, y, b, c)), d) =>
        make(R, y, make(B, z, a, b), make(B, elem, c, d))
      case _ => make(color, elem, left, right)
    }

    def balanceRight(color: Color, elem: Interval[E], left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
      case (B, a, RBTreeImpl(R, y, b, RBTreeImpl(R, _elem, _color, d))) =>
        make(R, y, make(B, elem, a, b), make(B, _elem, _color, d))
      case (B, a, RBTreeImpl(R, z, RBTreeImpl(R, y, b, _color), d)) =>
        make(R, y, make(B, elem, a, b), make(B, z, _color, d))
      case _ => make(color, elem, left, right)
    }

    def blacken(t: RBTree[E]) = make(B, t.value, t.left, t.right)

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

case class RBTreeImpl[E](color: Color,
                         value: Interval[E],
                         left: RBTree[E],
                         right: RBTree[E]) extends RBTree[E] {


  override def lookupOverlapping(key: Long): List[E] = {
    null.asInstanceOf
  }

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

  sealed trait Color

  case object R extends Color

  case object B extends Color

  /**
    * Creates a new red-black tree from given 'xs' sequence.
    *
    * Time - O(n log n)
    * Space - O(log n)
    */
  def apply[E](xs: Interval[E]*): RBTree[E] = {
    var r: RBTree[E] = empty[E]
    for (x <- xs) r = r.add(x)
    r
  }

  def empty[E]: RBTree[E] = {
    Leaf.asInstanceOf[RBTree[E]]
  }
}

/** *******************************************************************************************************************************************************************************************************************/


//package com.adform.task.scala_rb_tree
//
///**
//  * Created by vladislav.molchanov on 18.02.2016.
//  */
//
//
//import RBTree._
//
//abstract /*sealed*/ class RBTree[E <% Ordered[E]] {
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
//  protected def make(color: Color, elem: E,
//                     left: RBTree[E] = empty[E],
//                     right: RBTree[E] = empty[E]): RBTree[E] = {
//    RBTreeImpl(color, elem, left, right)
//  }
//
//  def add(elem: E): RBTree[E] = {
//
//    def balancedAdd(tree: RBTree[E]): RBTree[E] =
//      if (tree.isEmpty) make(R, elem)
//      else if (elem < tree.value) balanceLeft(tree.color, tree.value, balancedAdd(tree.left), tree.right)
//      else if (elem > tree.value) balanceRight(tree.color, tree.value, tree.left, balancedAdd(tree.right))
//      else tree
//
//    def balanceLeft(color: Color, elem: E, left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
//      case (B, RBTreeImpl(R, y, RBTreeImpl(R, z, a, b), c), d) =>
//        make(R, y, make(B, z, a, b), make(B, elem, c, d))
//      case (B, RBTreeImpl(R, z, a, RBTreeImpl(R, y, b, c)), d) =>
//        make(R, y, make(B, z, a, b), make(B, elem, c, d))
//      case _ => make(color, elem, left, right)
//    }
//
//    def balanceRight(color: Color, elem: E, left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
//      case (B, a, RBTreeImpl(R, y, b, RBTreeImpl(R, _elem, _color, d))) =>
//        make(R, y, make(B, elem, a, b), make(B, _elem, _color, d))
//      case (B, a, RBTreeImpl(R, z, RBTreeImpl(R, y, b, _color), d)) =>
//        make(R, y, make(B, elem, a, b), make(B, z, _color, d))
//      case _ => make(color, elem, left, right)
//    }
//
//    def blacken(t: RBTree[E]) = if (t.color == B) t else make(B, t.value, t.left, t.right)
//
//    val balanced: RBTree[E] = balancedAdd(this)
//    val _blacken: RBTree[E] = blacken(balanced)
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
//case class RBTreeImpl[E](color: Color,
//                         value: E,
//                         left: RBTree[E],
//                         right: RBTree[E])(implicit ev$1: E => Ordered[E]) extends RBTree[E] {
//
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
//  sealed trait Color
//
//  sealed case object R extends Color
//
//  sealed case object B extends Color
//
//  /**
//    * Creates a new red-black tree from given 'xs' sequence.
//    *
//    * Time - O(n log n)
//    * Space - O(log n)
//    */
//  def apply[A <% Ordered[A]](xs: A*): RBTree[A] = {
//    var r: RBTree[A] = empty[A]
//    for (x <- xs) r = r.add(x)
//    r
//  }
//
//  def empty[A](implicit ordering: Ordering[A]): RBTree[A] = {
//    Leaf.asInstanceOf[RBTree[A]]
//  }
//}
//
///** *******************************************************************************************************************************************************************************************************************/

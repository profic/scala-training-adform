//package com.adform.task.scala_rb_tree
//
///**
//  * Created by vladislav.molchanov on 18.02.2016.
//  */
//
//
//import RBTree._
//
//abstract sealed class RBTree[E] {
//
//  def lookupOverlapping(key: Long): List[E] = ???
//
//  protected def color: Color
//
//  def value: Interval[E]
//
//  def left: RBTree[E]
//
//  def right: RBTree[E]
//
//  def isEmpty: Boolean
//
//  protected def make(color: Color, elem: Interval[E],
//                     left: RBTree[E] = empty,
//                     right: RBTree[E] = empty): RBTree[E] = {
//    println("make")
//    RBTreeImpl(color, elem, left, right)
//  }
//
//  def add(elem: Interval[E]): RBTree[E] = {
//
//    def balancedAdd(tree: RBTree[E]): RBTree[E] =
//      if (tree.isEmpty) make(R, elem)
//      else {
//        if (elem < tree.value) balanceLeft(tree.color, tree.value, balancedAdd(tree.left), tree.right)
//        else if (elem > tree.value) balanceRight(tree.color, tree.value, tree.left, balancedAdd(tree.right))
//        else tree
//      }
//
//    def balanceLeft(color: Color, elem: Interval[E], left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
//      case (B, RBTreeImpl(R, y, RBTreeImpl(R, z, a, b), c), d) =>
//        make(R, y, make(B, z, a, b), make(B, elem, c, d))
//      case (B, RBTreeImpl(R, z, a, RBTreeImpl(R, y, b, c)), d) =>
//        make(R, y, make(B, z, a, b), make(B, elem, c, d))
//      case _ => make(color, elem, left, right)
//    }
//
//    def member[elt: Ordered](x: elt, ys: Set[elt]): Boolean = {
//      true
//    }
//
//    def balanceRight(color: Color, elem: Interval[E], left: RBTree[E], right: RBTree[E]) = (color, left, right) match {
//      case (B, a, RBTreeImpl(R, y, b, RBTreeImpl(R, _elem, _color, d))) =>
//        make(R, y, make(B, elem, a, b), make(B, _elem, _color, d))
//      case (B, a, RBTreeImpl(R, z, RBTreeImpl(R, y, b, _color), d)) =>
//        make(R, y, make(B, elem, a, b), make(B, z, _color, d))
//      case _ => make(color, elem, left, right)
//    }
//
//    def blacken(t: RBTree[E]) = make(B, t.value, t.left, t.right)
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
//                         value: Interval[E],
//                         left: RBTree[E],
//                         right: RBTree[E]) extends RBTree[E] {
//
//
//  override def lookupOverlapping(key: Long): List[E] = {
//    null.asInstanceOf
//  }
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
//  case object R extends Color
//
//  case object B extends Color
//
//  /**
//    * Creates a new red-black tree from given 'xs' sequence.
//    *
//    * Time - O(n log n)
//    * Space - O(log n)
//    */
//  def apply[E](xs: Interval[E]*): RBTree[E] = {
//    var r: RBTree[E] = empty[E]
//    for (x <- xs) r = r.add(x)
//    r
//  }
//
//  def empty[E]: RBTree[E] = {
//    Leaf.asInstanceOf[RBTree[E]]
//  }
//}

/** *******************************************************************************************************************************************************************************************************************/


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
abstract sealed class Tree[+A <% Ordered[A]] {

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
  def add[U >: A <% Ordered[U]](x: U): Tree[U] = {
    def balancedAdd(t: Tree[A]): Tree[U] =
      if (t.isEmpty) Tree.make(R, x)
      else if (x < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right)
      else if (x > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def balanceLeft(c: Color, x: A, l: Tree[U], r: Tree[A]) = (c, l, r) match {
      case (B, Branch(R, y, Branch(R, z, a, b), c), d) =>
        Tree.make(R, y, Tree.make(B, z, a, b), Tree.make(B, x, c, d))
      case (B, Branch(R, z, a, Branch(R, y, b, c)), d) =>
        Tree.make(R, y, Tree.make(B, z, a, b), Tree.make(B, x, c, d))
      case _ => Tree.make(c, x, l, r)
    }

    def balanceRight(c: Color, x: A, l: Tree[A], r: Tree[U]) = (c, l, r) match {
      case (B, a, Branch(R, y, b, Branch(R, z, c, d))) =>
        Tree.make(R, y, Tree.make(B, x, a, b), Tree.make(B, z, c, d))
      case (B, a, Branch(R, z, Branch(R, y, b, c), d)) =>
        Tree.make(R, y, Tree.make(B, x, a, b), Tree.make(B, z, c, d))
      case _ => Tree.make(c, x, l, r)
    }

    def blacken(t: Tree[U]):Tree[U] = Tree.make(B, t.value, t.left, t.right)

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

case class Branch[A <% Ordered[A]](color: Color,
                                   value: A,
                                   left: Tree[A],
                                   right: Tree[A]) extends Tree[A] {
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

  /**
    * Returns an empty red-black tree instance.
    *
    * Time - O(1)
    * Space - O(1)
    */
  def empty[A]: Tree[A] = Leaf

  /**
    *
    */
  def make[A <% Ordered[A]](c: Color, x: A, l: Tree[A] = Leaf, r: Tree[A] = Leaf): Tree[A] =
    Branch(c, x, l, r)

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
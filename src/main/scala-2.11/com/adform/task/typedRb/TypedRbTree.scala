package com.adform.task.typedRb

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */

abstract sealed class Color

case object R extends Color

case object B extends Color


abstract sealed class TreeTemplate[+A](implicit ev1: A ⇒ Ordered[A]) {

  type Upper

  def color: Color

  def value: A

  def left: TreeTemplate[A]

  def right: TreeTemplate[A]

  def isEmpty: Boolean


  protected def make[U >: A <: Upper](c: Color, v: U, l: TreeTemplate[U], r: TreeTemplate[U])(implicit ev: U => Ordered[U]): TreeTemplate[U]

  def add[U >: A <: Upper](elem: U)(implicit ev1: U ⇒ Ordered[U]): TreeTemplate[U] = {

    type T = TreeTemplate[U]

    def balancedAdd(t: TreeTemplate[A]): TreeTemplate[U] = //balancedAdd - just adds element to the left or right (like in any binary tree)
      if (t.isEmpty) make(R, elem, t, t)
      else if (elem < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right) // balanceLeft or balanceRight - balances tree
      else if (elem > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    def rotate(z: U, y: U, x: U, a: TreeTemplate[U], b: TreeTemplate[U], c: TreeTemplate[U], d: TreeTemplate[U]): TreeTemplate[U] = {
      make(R, y, make(B, x, a, b), make(B, z, c, d))
    }

    def balanceLeft(c: Color, z: A, l: T, r: T) = (c, l, r) match {
      case (B, TreeTemplate(R, y, TreeTemplate(R, x, a, b), c), d) => rotate(z, y, x, a, b, c, d)
      case (B, TreeTemplate(R, x, a, TreeTemplate(R, y, b, c)), d) => rotate(z, y, x, a, b, c, d)
      case _                                                       => make(c, z, l, r)
    }

    def balanceRight(c: Color, x: U, l: T, r: T) = (c, l, r) match {
      case (B, a, TreeTemplate(R, y, b, TreeTemplate(R, z, c, d))) => rotate(z, y, x, a, b, c, d)
      case (B, a, TreeTemplate(R, z, TreeTemplate(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
      case (B, a, TreeTemplate(R, z, TreeTemplate(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
      case _                                                       => make(c, x, l, r)
    }

    def blacken(t: T): T = make(B, t.value, t.left, t.right)

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

abstract sealed class Tree[+A](implicit ev1: A ⇒ Ordered[A]) extends TreeTemplate[A] {

  override protected def make[U >: A <: Any](c: Color, v: U, l: TreeTemplate[U], r: TreeTemplate[U])(implicit ev: (U) => Ordered[U]): TreeTemplate[U] = {
    T(c, v, l, r)
  }

}

abstract class IntervalTree[A, I](implicit ev1: I => Ordered[I]) extends TreeTemplate[Interval[A, I]] {

  type interval = Interval[A, I]

  override type Upper = Interval[A, I]

  override protected def make[U >: Interval[A, I] <: Interval[A, I]](c: Color, v: U,
                                                                     l: TreeTemplate[U],
                                                                     r: TreeTemplate[U])(implicit ev2: (U) => Ordered[U]): IntervalTree[A, I] = {

    In(c, v, l, r)
  }
}

case class In[A, I](color: Color,
                    value: Interval[A, I],
                    left: TreeTemplate[Interval[A, I]] = Leaf.asInstanceOf,
                    right: TreeTemplate[Interval[A, I]] = Leaf.asInstanceOf)(implicit ev1: I ⇒ Ordered[I]) extends IntervalTree[A, I] {

  def isEmpty = false

}

case class Interval[A, I](begin: I, end: I, data: A)
                         (implicit ev1: I ⇒ Ordered[I]) extends Ordered[Interval[A, I]] {

  override def compare(that: Interval[A, I]): Int = {
    val res: Int = begin compare that.begin
    if (res != 0) res else end compare that.end
  }
}


case class T[A](color: Color,
                value: A,
                left: TreeTemplate[A] = Leaf.asInstanceOf[TreeTemplate[A]],
                right: TreeTemplate[A] = Leaf.asInstanceOf[TreeTemplate[A]])(implicit ev: A => Ordered[A]) extends Tree[A] {
  def isEmpty = false

}

case object Leaf extends IntervalTree[Nothing, Nothing] {
  def color: Color = B

  def value: Nothing = fail("An empty tree.")

  def left: IntervalTree[Nothing, Nothing] = fail("An empty tree.")

  def right: IntervalTree[Nothing, Nothing] = fail("An empty tree.")

  def isEmpty = true
}

object TreeTemplate {

  type T[U] = TreeTemplate[U]

  def unapply[U](arg: T[U]): Option[(Color, U, T[U], T[U])] = {
    Some((arg.color, arg.value, arg.left, arg.right))
  }

  //  def empty[A]: Tree[A] = Leaf.asInstanceOf

  def main(args: Array[String]) {
    val tree = apply(Interval(1, 10, "hello"))

    print(tree)
  }


  def apply[A, I <% Ordered[I]](xs: Interval[A, I]*): IntervalTree[A, I] = {

    //    val r: IntervalTree[A, I] = Leaf.asInstanceOf[IntervalTree[A, I]]
    //
    //    for (x <- xs) {
    //      r.add(x)
    //    }
    //    //    r
    //    r
    ???
  }
}


//package com.adform.task.typedRb
//
///**
//  * Created by vladislav.molchanov on 18.02.2016.
//  */
//
//abstract sealed class Color
//
//case object R extends Color
//
//case object B extends Color
//
//
//abstract sealed class TreeTemplate[+A, Upper](implicit ev1: A ⇒ Ordered[A]) {
//
//  def color: Color
//
//  def value: A
//
//  def left: TreeTemplate[A, Upper]
//
//  def right: TreeTemplate[A, Upper]
//
//  def isEmpty: Boolean
//
//
//  protected def make[U >: A <: Upper](c: Color, v: U, l: TreeTemplate[U, Upper], r: TreeTemplate[U, Upper])(implicit ev: U => Ordered[U]): TreeTemplate[U, Upper]
//
//  def add[U >: A <: Upper](elem: U)(implicit ev1: U ⇒ Ordered[U]): TreeTemplate[U, Upper] = {
//
//    type T = TreeTemplate[U, Upper]
//
//    def balancedAdd(t: TreeTemplate[A, Upper]): TreeTemplate[U, Upper] = //balancedAdd - just adds element to the left or right (like in any binary tree)
//      if (t.isEmpty) make(R, elem, t, t)
//      else if (elem < t.value) balanceLeft(t.color, t.value, balancedAdd(t.left), t.right) // balanceLeft or balanceRight - balances tree
//      else if (elem > t.value) balanceRight(t.color, t.value, t.left, balancedAdd(t.right))
//      else t
//
//    def rotate(z: U, y: U, x: U, a: TreeTemplate[U, Upper], b: TreeTemplate[U, Upper], c: TreeTemplate[U, Upper], d: TreeTemplate[U, Upper]): TreeTemplate[U, Upper] = {
//      make(R, y, make(B, x, a, b), make(B, z, c, d))
//    }
//
//    def balanceLeft(c: Color, z: A, l: T, r: T) = (c, l, r) match {
//      case (B, TreeTemplate(R, y, TreeTemplate(R, x, a, b), c), d) => rotate(z, y, x, a, b, c, d)
//      case (B, TreeTemplate(R, x, a, TreeTemplate(R, y, b, c)), d) => rotate(z, y, x, a, b, c, d)
//      case _                                             => make(c, z, l, r)
//    }
//
//    def balanceRight(c: Color, x: U, l: T, r: T) = (c, l, r) match {
//      case (B, a, TreeTemplate(R, y, b, TreeTemplate(R, z, c, d))) => rotate(z, y, x, a, b, c, d)
//      case (B, a, TreeTemplate(R, z, TreeTemplate(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
//      case (B, a, TreeTemplate(R, z, TreeTemplate(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
//      case _                                             => make(c, x, l, r)
//    }
//
//    def blacken(t: T): T = make(B, t.value, t.left, t.right)
//
//    blacken(balancedAdd(this))
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
//abstract sealed class Tree[+A](implicit ev1: A ⇒ Ordered[A]) extends TreeTemplate[A, Any] {
//
//  override protected def make[U >: A <: Any](c: Color, v: U, l: TreeTemplate[U, Any], r: TreeTemplate[U, Any])(implicit ev: (U) => Ordered[U]): TreeTemplate[U, Any] = {
//    T(c, v, l, r)
//  }
//
//}
//
//abstract class IntervalTree[A, I](implicit ev1: I => Ordered[I]) extends TreeTemplate[Interval[A, I], Interval[A, I]] {
//
//  type interval = Interval[A, I]
//
//  override protected def make[U >: Interval[A, I] <: Interval[A, I]](c: Color, v: U,
//                                                                     l: TreeTemplate[U, Interval[A, I]],
//                                                                     r: TreeTemplate[U, Interval[A, I]])(implicit ev2: (U) => Ordered[U]): IntervalTree[A, I] = {
//
//    In(c, v, l, r)
//  }
//}
//
//case class In[A, I](color: Color,
//                    value: Interval[A, I],
//                    left: TreeTemplate[Interval[A, I], Interval[A, I]] = Leaf.asInstanceOf,
//                    right: TreeTemplate[Interval[A, I], Interval[A, I]] = Leaf.asInstanceOf)(implicit ev1: I ⇒ Ordered[I]) extends IntervalTree[A, I] {
//
//  def isEmpty = false
//
//}
//
//case class Interval[A, I](begin: I, end: I, data: A)
//                         (implicit ev1: I ⇒ Ordered[I]) extends Ordered[Interval[A, I]] {
//
//  override def compare(that: Interval[A, I]): Int = {
//    val res: Int = begin compare that.begin
//    if (res != 0) res else end compare that.end
//  }
//}
//
//
//case class T[A](color: Color,
//                value: A,
//                left: TreeTemplate[A, Any] = Leaf.asInstanceOf[TreeTemplate[A, Any]],
//                right: TreeTemplate[A, Any] = Leaf.asInstanceOf[TreeTemplate[A, Any]])(implicit ev: A => Ordered[A]) extends Tree[A] {
//  def isEmpty = false
//
//}
//
//case object Leaf extends IntervalTree[Nothing, Nothing] {
//  def color: Color = B
//
//  def value: Nothing = fail("An empty tree.")
//
//  def left: IntervalTree[Nothing, Nothing] = fail("An empty tree.")
//
//  def right: IntervalTree[Nothing, Nothing] = fail("An empty tree.")
//
//  def isEmpty = true
//}
//
//object TreeTemplate {
//
//  type T[U, Upper] = TreeTemplate[U, Upper]
//
//  def unapply[U, Upper](arg: T[U, Upper]): Option[(Color, U, T[U, Upper], T[U, Upper])] = {
//    Some((arg.color, arg.value, arg.left, arg.right))
//  }
//
//  //  def empty[A, Upper]: Tree[A, Upper] = Leaf.asInstanceOf
//
//  def main(args: Array[String]) {
//    val tree = apply(Interval(1, 10, "hello"))
//
//    print(tree)
//  }
//
//
//  def apply[A, I <% Ordered[I]](xs: Interval[A, I]*): IntervalTree[A, I] = {
//
//    val r: IntervalTree[A, I] = Leaf.asInstanceOf[IntervalTree[A, I]]
//
//    for (x <- xs) {
//      r.add(x)
//    }
//    //    r
//    r
//  }
//}
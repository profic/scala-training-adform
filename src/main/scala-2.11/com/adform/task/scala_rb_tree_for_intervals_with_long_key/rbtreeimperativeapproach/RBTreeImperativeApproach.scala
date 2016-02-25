package com.adform.task.scala_rb_tree_for_intervals_with_long_key.rbtreeimperativeapproach


abstract sealed class Color

case object R extends Color

case object B extends Color

/**
  * A Red-Black Tree.
  */

abstract sealed class RBTreeImperativeApproach[A] {

  type T = RBTreeImperativeApproach[A]

  type I = Interval[A]

  def min: Long

  def max: Long

  def color: Color

  def value: I

  def left: T

  def right: T

  def isEmpty: Boolean

  def search(key: Long): List[A] = {

    def overlaps(a: Long, b: Long): Boolean = {
      key.compare(a) >= 0 && key.compare(b) <= 0
    }

    // JMH shows that using ArrayBuffer can speed up to 2x-3x times compare to List

    /*val as = new ArrayBuffer[A]()

    def loop(tree: Tree[A]): Unit = {

      if (tree != Leaf) {
        if (tree.left != Leaf && overlaps(tree.left.min, tree.left.max)) loop(tree.left)
        if (overlaps(tree.value.begin, tree.value.end)) {
          as += tree.value.data
        }
        if (tree.right != Leaf && overlaps(tree.right.min, tree.right.max)) loop(tree.right)
      }
    }
    loop(this)
    as.toList
*/
    def loop(tree: RBTreeImperativeApproach[A], acc: List[A]): List[A] = {
      if (tree == Leaf) acc
      else {
        val rightRes = if (tree.left != Leaf && overlaps(tree.left.min, tree.left.max)) loop(tree.left, acc) else acc
        val theesRes = if (overlaps(tree.value.begin, tree.value.end)) tree.value.data :: acc else acc
        val leftRes = if (tree.right != Leaf && overlaps(tree.right.min, tree.right.max)) loop(tree.right, acc) else acc
        rightRes ::: theesRes ::: leftRes
      }
    }
    loop(this, List())
  }


  def add(elem: I): T = {

    def balance(color: Color, el: I, l: T, r: T) = (color, l, el, r) match {


      //    B (T R a (T R _ _ _) x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
      case (B, T(R, a@T(R, _, _, _), x, b), y, T(R, c, z, d)) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      //    B (T R a x b (T R _ _ _)) y (T R c z d) = T R (T B a x b) y (T B c z d)
      case (B, T(R, a, x, b@T(R, _, _, _)), y, T(R, c, z, d)) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      //    B (T R a x b) y (T R c (T R _ _ _) z d) = T R (T B a x b) y (T B c z d)
      case (B, T(R, a, x, b), y, T(R, c@T(R, _, _, _), z, d)) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      //    B (T R a x b) y (T R c z d (T R _ _ _)) =         T R (T B a x b) y (T B c z d)
      case (B, T(R, a, x, b), y, T(R, c, z, d@T(R, _, _, _))) => T(R, T(B, a, x, b), y, T(B, c, z, d))
      //    B (T R a (T R _ _ _) x b) y c = T B a x (T R b y c)
      case (B, T(R, a@T(R, _, _, _), x, b), y, c) => T(B, a, x, T(R, b, y, c))
      //    B a x (T R b y c (T R _ _ _)) = T B (T R a x b) y c
      case (B, a, x, T(R, b, y, c@T(R, _, _, _))) => T(B, T(R, a, x, b), y, c)
      //    B (T R a x (T R b y c)) z d =       T B (T R a x b) y (T R c z d)
      case (B, T(R, a, x, T(R, b, y, c)), z, d) => T(B, T(R, a, x, b), y, T(R, c, z, d))
      //    B a x (T R (T R b y c) z d) = T B (T R a x b) y (T R c z d)
      case (B, a, x, T(R, T(R, b, y, c), z, d)) => T(B, T(R, a, x, b), y, T(R, c, z, d))
      //    balance color a x b = T color a x b
      case (c, a, x, b) => T(c, a, x, b)


    }


    def balancedAdd(t: RBTreeImperativeApproach[A]): RBTreeImperativeApproach[A] =
      if (t.isEmpty) T(R, t, elem, t)
      else if (elem < t.value) balance(t.color, t.value, balancedAdd(t.left), t.right)
      else if (elem > t.value) balance(t.color, t.value, t.left, balancedAdd(t.right))
      else t

    //    def rotate(z: I, y: I, x: I, a: T, b: T, c: T, d: T): T = {
    //      T(R, y, T(B, x, a, b), T(B, z, c, d))
    //    }
    //    def balanceLeft(c: Color, z: I, l: T, r: T) = (c, l, r) match {
    //      case (B, T(R, y, T(R, x, a, b), c), d) => rotate(z, y, x, a, b, c, d)
    //      case (B, T(R, x, a, T(R, y, b, c)), d) => rotate(z, y, x, a, b, c, d)
    //      case _                                 => T(c, z, l, r)
    //    }
    //
    //    def balanceRight(c: Color, x: I, l: T, r: T) = (c, l, r) match {
    //      case (B, a, T(R, y, b, T(R, z, c, d))) => rotate(z, y, x, a, b, c, d)
    //      case (B, a, T(R, z, T(R, y, b, c), d)) => rotate(z, y, x, a, b, c, d)
    //      case _                                 => T(c, x, l, r)
    //    }

    def blacken(t: T): T = T(B, t.left, t.value, t.right)

    blacken(balancedAdd(this))
  }

  def fail(m: String) = throw new NoSuchElementException(m)
}

case class Interval[A](begin: Long, end: Long, data: A) extends Ordered[Interval[A]] {

  override def compare(that: Interval[A]): Int = {
    val res: Int = begin compare that.begin
    if (res != 0) res else end compare that.end
  }
}

case class T[A](color: Color,
                left: RBTreeImperativeApproach[A],
                value: Interval[A],
                right: RBTreeImperativeApproach[A]) extends RBTreeImperativeApproach[A] {
  def isEmpty = false

  val _min: Long =
    if (left == Leaf) value.begin
    else left.min

  val _max: Long = {
    val leftMax: Long = if (left != Leaf) math.max(left.max, value.end) else value.end
    val rightMax: Long = if (right != Leaf) math.max(right.max, value.end) else value.end
    math.max(leftMax, rightMax)
  }

  override def min: Long = _min

  override def max: Long = _max
}

case object Leaf extends RBTreeImperativeApproach[Nothing] {
  def color: Color = B

  def value = fail("An empty tree.")

  def left = fail("An empty tree.")

  def right = fail("An empty tree.")

  def isEmpty = true

  override def min = fail("An empty tree.")

  override def max = fail("An empty tree.")
}

object RBTreeImperativeApproach {

  def main(args: Array[String]) {

    val tree12 = RBTreeImperativeApproach(
      Interval(1, 4, "1"),
      Interval(4, 11, "2"),
      Interval(5, 10, "3"),
      Interval(3, 9, "4"),
      Interval(2, 5, "5"),
      Interval(0, 12, "6")
    )

    println(tree12.search(6))
  }

  def empty[A]: RBTreeImperativeApproach[A] = Leaf.asInstanceOf[RBTreeImperativeApproach[A]]

  def apply[A](xs: Interval[A]*): RBTreeImperativeApproach[A] = {
    var r = empty[A]
    for (x <- xs) r = r.add(x)
    r
  }
}
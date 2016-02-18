package com.adform.task.scala_rb_tree

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */
class IntervalRBTreeNode[E](color: Color,
                            value: E,
                            begin: Int,
                            end: Int,
                            left: RBTree[Interval[E]],
                            right: RBTree[Interval[E]]) extends RBNode[Interval[E]](color, Interval(begin, end, value), left, right) {

  override protected def make(color: Color, elem: Interval[E], left: RBTree[Interval[E]], right: RBTree[Interval[E]]): RBTree[Interval[E]] = {
    super.make(color, elem, left, right)
  }
}


case class Interval[E](begin: Int, end: Int, data: E) extends Ordered[Interval[E]] {

  override def compare(that: Interval[E]): Int = this.begin compare that.begin

}


case class RBNode2(color: Color,
                   value: Interval2,
                   left: RBTree[Interval2],
                   right: RBTree[Interval2]) extends RBTree[Interval2] {
  def isEmpty = false

}

case class Interval2(begin: Int, end: Int) extends Ordered[Interval2] {

  override def compare(that: Interval2): Int = 0

}

object IntervalTreeTest {

  def main(args: Array[String]) {


    //    tree.insert(new Interval(1, 10), "111")
    //    tree.insert(new Interval(2, 5), "222")
    //    tree.insert(new Interval(6, 100), "333")
    //    tree.insert(new Interval(1, 4), "333")
    //    tree.insert(new Interval(7, 9), "333")
  }
}
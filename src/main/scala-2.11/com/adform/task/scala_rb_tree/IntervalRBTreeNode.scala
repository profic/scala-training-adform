package com.adform.task.scala_rb_tree

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */

case class Interval[E](begin: Long, end: Long, data: E) extends Ordered[Interval[E]] {

  override def compare(that: Interval[E]): Int = {

    if (this.begin != that.begin) this.begin compare that.begin
    else {
      this.end compare that.end
    }
  }
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
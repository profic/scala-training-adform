package com.adform.task.scala_rb_tree

/**
  * Created by vladislav.molchanov on 18.02.2016.
  */

case class Interval[E](begin: Long, end: Long, data: E) extends Ordered[Interval[E]] {

  override def compare(that: Interval[E]): Int = {
    if (this.begin != that.begin) this.begin compare that.begin
    else this.end compare that.end
  }
}

package com.adform.task.benchmark

import com.adform.task.scala_rb_tree_for_intervals.Interval


/**
  * Created by vladislav.molchanov on 23.02.2016.
  */
object IntervalCreationBenchmarkScala {

  def create(): Unit = {
    Interval[String, Long](1,2,"")
  }

}
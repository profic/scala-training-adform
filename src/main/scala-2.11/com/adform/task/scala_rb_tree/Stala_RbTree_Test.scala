package com.adform.task.scala_rb_tree


/**
  * Created by vladislav.molchanov on 18.02.2016.
  */
object Stala_RbTree_Test {

  def main(args: Array[String]) {
    val tree: RBTree[String] = RBTree(
      Interval(1, 10, "111"),
      Interval(5, 11, "222"),
      Interval(1, 4, "333"),
      Interval(6, 9, "444")
    )

    println(tree)
  }
}

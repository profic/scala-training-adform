package com.adform.task.scalaz_tree

import scalaz.Tree

/**
  * Created by vladislav.molchanov on 22.02.2016.
  */
object Scalaz_Tree_Example {

  def main(args: Array[String]) {
    import scalaz.Tree._

    val leaf: Tree[Int] = Leaf(1)
    val node: Tree[Int] = Node(2, Stream(leaf))

    node match {
      case l@Leaf(x)    => print("leaf")
      case n@Node(x, s) => print("node")
    }

    def asd[T](h: Holder[T]) = h match {
      case E()           =>
      case HolderImpl(v) =>
    }

  }

  abstract class Holder[T]

  object HolderImpl {
    def unapply[T](arg: HolderImpl[T]): Option[HolderImpl[T]] = Some(arg.value)
  }

  class HolderImpl[T] extends Holder[T] {

    private var _value = null.asInstanceOf[HolderImpl[T]]

    def this(value: HolderImpl[T]) {
      this()
      _value = value
    }

    def value = _value

  }

  case object EmptyHolder extends Holder[Nothing]

  case class E() extends HolderImpl[Nothing]() {
    def unapply(arg: E): Option[Nothing] = None
  }

}

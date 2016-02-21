package com.adform.task.scala_rb_tree

import com.twitter.algebird.Monoid
import scala.collection.immutable.MapLike
import scala.collection.mutable

/**
  * Created by Vlad on 21.02.2016.
  */
object Ttttt {


}


trait Foo

trait ReadableFoo extends Foo {
  def field: Int
}

case class Bar[+F <: Foo](foo: F) {
  def readField(implicit evidence: F <:< ReadableFoo) = foo.field
}

case class Grill[+B <: Bar[_]](bar: B) {
  def readField(implicit evidence: B <:< Bar[ReadableFoo]) = evidence(bar).readField
}

class ASD {

  abstract class T {
    def read
  }

  class Q[+F <: T](t:T) {
    def read(implicit ev:F <:< T) = t.read
  }

  class R[+F <: Q[T]](r:F){
    def read(implicit ev:F <:< Q[T]) = r.read
  }

}

//  todo: не работает
//case class Grill[+B <: Bar[_]](bar: B) {
//  def readField(implicit evidence: B <:< Bar[ReadableFoo]) = bar.readField
//}

// todo: не работает, но должно, т.к. Bar ковариантен к Foo, т.е. любой инстанс или наследник Bar буедт подтипом Bar[Foo]
//case class Grill[+F <: Foo, +B <: Bar[F]](bar: B) {
//  def readField(implicit evidence: F <:< ReadableFoo) = bar.readField
//}









//sealed trait Foo
//
//sealed trait ReadableFoo extends Foo {
//  def field: Int
//}
//
//trait NonReadableFoo extends Foo
//
//sealed trait BarM {
//
//  type F
//
//  def foo: F
//
//  def readField(implicit ev: F <:< ReadableFoo) = ev(foo).field
//}
//
//case class BarMI(override val foo: BarMI#F) extends BarM {
//  override type F <: ReadableFoo
//}
//
//
//sealed trait GrillT {
//
//  type B
//
//  def bar: B
//
//  def readBarField(implicit ev1: B <:< BarM) = ev1(bar).readField
//
//}
//
//case class Grill5(bar: Grill5#B) extends GrillT {
//
//  override type B <: BarM
//
//}
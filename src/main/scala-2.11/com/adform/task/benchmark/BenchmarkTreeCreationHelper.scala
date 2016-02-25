package com.adform.task.benchmark

import java.net.InetAddress

import com.adform.task.Main._
import com.adform.task.scala_rb_tree_for_intervals.{Interval, Tree}

import scala.io.Source


/**
  * Created by vladislav.molchanov on 25.02.2016.
  */
object BenchmarkTreeCreationHelper {

  def readResource(path: String): Source = Source.fromFile(getPath(path))

  def ipToLong(ip: InetAddress): Long = {
    val octets: Array[Byte] = ip.getAddress
    var result: Long = 0
    for (octet <- octets) {
      result <<= 8
      result |= octet & 0xff
    }
    result
  }

  val s = System.currentTimeMillis()

  val ip = ipToLong(InetAddress.getByName("92.173.0.104"))

  val rangesSource = readResource("/ranges.tsv")
  val rangesLines = rangesSource.getLines().toList

  var rangesLinesSize = rangesLines.size

  var i = 0

  val spl = rangesLines.map(_.split("-|\t")).map(splitted => {
    val rangeBegin = ipToLong(InetAddress.getByName(splitted(0)))
    val rangeEnd = ipToLong(InetAddress.getByName(splitted(1)))
    val networkName = splitted(2)
    (rangeBegin, rangeEnd, networkName)
  })

  def createTreeFunctional(): Unit = {
    spl.foldLeft(Tree[String, Long]())((tree, splitted) => {
        tree.add(Interval(splitted._1, splitted._2, splitted._3))
      })
  }

  def createTreeImperative(): Unit = {
    spl.foldLeft(Tree[String, Long]())((tree, splitted) => {
        tree.imperativeAdd(Interval(splitted._1, splitted._2, splitted._3))
      })
  }

  def main(args: Array[String]) {
    val tree: Tree[String, Long] = spl.foldLeft(Tree[String, Long]())((tree, splitted) => {
      tree.imperativeAdd(Interval(splitted._1, splitted._2, splitted._3))
    })
    val res: List[String] = tree.searchListTailRecursive(ip)
    println(res.size)
    println(Tree.creationCount)
  }

}

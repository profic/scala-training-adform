package com.adform.task

import java.io.Closeable
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import com.adform.task.intervalTree1.Interval

import scala.collection.mutable
import scala.io.Source
import scala.language.higherKinds

/**
  * Created by vladislav.molchanov on 16.02.2016.
  */
object Main2 extends {

  class NetworkInterval(begin: Double, end: Double, val networkName: String) extends Interval(begin, end)

  def main(args: Array[String]) {

    def getPath(path: String) = getClass.getResource(path).toURI
    def readResource(path: String): Source = Source.fromFile(getPath(path))

    val transactionsSource = readResource("/transactions.tsv")
    val transactionsLines: Iterator[String] = transactionsSource.getLines()

    val transactions = transactionsLines.map(_.split("\t"))
      .foldLeft(mutable.Map[String, List[String]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap(userId) = ip :: accMap(userId)
        accMap
      }).toMap.par

    val rangesSource = readResource("/ranges.tsv")
    val rangesLines: Iterator[String] = rangesSource.getLines()

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(List[NetworkInterval]())((acc, splitted) => {
        val rangeBegin: String = splitted(0)
        val rangeEnd: String = splitted(1)
        val networkName = splitted(2)
        val res: List[NetworkInterval] = new NetworkInterval(ipToLong(InetAddress.getByName(rangeBegin)), ipToLong(InetAddress.getByName(rangeEnd)), networkName) :: acc
        res
      })

    println("ranges splitted")

    // todo:improve
    implicit val ord:Ordering[NetworkInterval] = new Ordering[NetworkInterval] {
      override def compare(x: NetworkInterval, y: NetworkInterval): Int = intervalTree1.IntervalTree.intervalOrd.compare(x, y)
    }

    val tree = intervalTree1.IntervalTree(ranges)

    //    val tree: IntervalTree = IntervalTree()

    println("tree builded")

    val path: Path = Paths.get(getPath("/"))

    val output = Paths.get(path.toString, "output.tsv")
    if (Files.notExists(output)) {
      Files.createFile(output)
    }

    SimpleARM2(Files.newBufferedWriter(output))(writer => {
      for {
        (userId, ips) <- transactions
        ip <- ips
        interval <- tree.findContainingIntervals(ipToLong(InetAddress.getByName(ip)))
      } {
        writer.write(s"$userId\t${interval.networkName}\n")
      }
    })
  }

  private def ipToLong(ip: InetAddress): Long = {
    val octets: Array[Byte] = ip.getAddress
    var result: Long = 0
    for (octet <- octets) {
      result <<= 8
      result |= octet & 0xff
    }
    result
  }

  def isValidRange(ipStart: String, ipEnd: String, ipToCheck: String): Boolean = {
    val ipLo = ipToLong(InetAddress.getByName(ipStart))
    val ipHi = ipToLong(InetAddress.getByName(ipEnd))
    val ipToTest = ipToLong(InetAddress.getByName(ipToCheck))
    ipToTest >= ipLo && ipToTest <= ipHi
  }
}

object SimpleARM2 {
  def apply[T <: Closeable, Q](c: T)(f: (T) => Q): Q = {
    try {
      f(c)
    } finally {
      c.close()
    }
  }
}
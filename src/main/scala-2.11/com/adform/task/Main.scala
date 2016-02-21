package com.adform.task

import java.io._
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import com.adform.task.scala_rb_tree_for_intervals.{Interval, Tree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.higherKinds

/**
  * Created by vladislav.molchanov on 16.02.2016.
  */
object Main extends {

  def main(args: Array[String]) {

    //    bruteForce()
    //    tree()


    val rangesSource = readResource("/ranges copy.tsv")
    val rangesLines = rangesSource.getLines().toList

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(Tree[String, Long]())((tree, splitted) => {

        val rangeBegin = ipToLong(InetAddress.getByName(splitted(0)))
        val rangeEnd = ipToLong(InetAddress.getByName(splitted(1)))
        val networkName = splitted(2)

        tree.add(Interval(rangeBegin, rangeEnd, networkName))
      })

    val start: Long = System.nanoTime()

    val res: List[String] = ranges.search(ipToLong(InetAddress.getByName("92.173.0.104")))

    print(System.nanoTime() - start)

  }

  def getPath(path: String) = getClass.getResource(path).toURI

  def readResource(path: String): Source = Source.fromFile(getPath(path))

  def tree() = {

    val transactionsSource = readResource("/transactions.tsv")
    val transactionsLines = transactionsSource.getLines().toList

    val transactions: Map[String, List[String]] = transactionsLines.map(_.split("\t"))
      .foldLeft(mutable.Map[String, List[String]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap(userId) = ip :: accMap(userId)
        accMap
      }).toMap

    val rangesSource = readResource("/ranges.tsv")
    val rangesLines = rangesSource.getLines().toList

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(Tree[String, Long]())((tree, splitted) => {

        val rangeBegin = ipToLong(InetAddress.getByName(splitted(0)))
        val rangeEnd = ipToLong(InetAddress.getByName(splitted(1)))
        val networkName = splitted(2)

        tree.add(Interval(rangeBegin, rangeEnd, networkName))
      })

    type Network = String

    println("ranges splitted")

    val path: Path = Paths.get(getPath("/"))

    val output = Paths.get(path.toString, "output.tsv")
    if (Files.notExists(output)) {
      Files.createFile(output)
    }

    SimpleARM(Files.newBufferedWriter(output))(writer => {
      for {
        (userId, ips) <- transactions
        ip <- ips
        network ← ranges.search(ipToLong(InetAddress.getByName(ip)))
      } {
        writer.write(s"$userId\t$network\n")
      }
    })
  }

  def bruteForce() = {
    def getPath(path: String) = getClass.getResource(path).toURI
    def readResource(path: String): Source = Source.fromFile(getPath(path))

    val transactionsSource = readResource("/transactions.tsv")
    val transactionsLines = transactionsSource.getLines().toList

    val transactions: Map[String, List[String]] = transactionsLines.map(_.split("\t"))
      .foldLeft(mutable.Map[String, List[String]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap(userId) = ip :: accMap(userId)
        accMap
      }).toMap

    val rangesSource = readResource("/ranges.tsv")
    val rangesLines = rangesSource.getLines().toList


    type Network = String

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(List[(String, String, Network)]())((acc, splitted) => {
        val rangeBegin = splitted(0)
        val rangeEnd = splitted(1)
        val networkName = splitted(2)

        (rangeBegin, rangeEnd, networkName) :: acc
      })

    println("ranges splitted")

    val path: Path = Paths.get(getPath("/"))

    val output = Paths.get(path.toString, "output.tsv")
    if (Files.notExists(output)) {
      Files.createFile(output)
    }


    var i = 0
    var j = 0

    var iStrings = ArrayBuffer[String]()
    var jStrings = ArrayBuffer[String]()

    println(s"transactions size: ${transactions.size}")
    println(s"ranges size: ${ranges.size}")

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

  def isValidRange(ipStart: Long, ipEnd: Long, ipToCheck: String): Boolean = {
    val ipToTest = ipToLong(InetAddress.getByName(ipToCheck))
    ipToTest >= ipStart && ipToTest <= ipEnd
  }

  def isValidRange(ipStart: Long, ipEnd: Long, ipToTest: Long): Boolean = {
    ipToTest >= ipStart && ipToTest <= ipEnd
  }

  def isValidRange(ipStart: String, ipEnd: String, ipToCheck: String): Boolean = {
    val ipLo = ipToLong(InetAddress.getByName(ipStart))
    val ipHi = ipToLong(InetAddress.getByName(ipEnd))
    val ipToTest = ipToLong(InetAddress.getByName(ipToCheck))
    ipToTest >= ipLo && ipToTest <= ipHi
  }
}

object SimpleARM {
  def apply[T <: Closeable, Q](c: T)(f: (T) => Q): Q = {
    try {
      f(c)
    } finally {
      c.close()
    }
  }
}
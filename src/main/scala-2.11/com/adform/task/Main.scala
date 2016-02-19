package com.adform.task

import java.io.Closeable
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import com.adform.task.scala_rb_tree.{Interval, RBTree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.higherKinds

/**
  * Created by vladislav.molchanov on 16.02.2016.
  */
object Main extends {

  def main(args: Array[String]) {

    def getPath(path: String) = getClass.getResource(path).toURI
    def readResource(path: String): Source = Source.fromFile(getPath(path))

    val transactionsSource = readResource("/transactions.tsv")
    val transactionsLines = transactionsSource.getLines().toList

    val transactions: Map[String, List[Long]] = transactionsLines.map(_.split("\t"))
      .foldLeft(mutable.Map[String, List[Long]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap(userId) = ipToLong(InetAddress.getByName(ip)) :: accMap(userId)
        accMap
      }).toMap

    val rangesSource = readResource("/ranges.tsv")
    val rangesLines = rangesSource.getLines().toList

    //    val ranges = rangesLines.map(_.split("-|\t"))
    //      .foldLeft(RBTree[String]())((tree, splitted) => {
    //
    //        val rangeBegin = ipToLong(InetAddress.getByName(splitted(0)))
    //        val rangeEnd = ipToLong(InetAddress.getByName(splitted(1)))
    //        val networkName = splitted(2)
    //
    //        tree.add(Interval(rangeBegin, rangeEnd, networkName))
    //      })

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(List[(Long, Long, String)]())((acc, splitted) => {
        val rangeBegin: Long = ipToLong(InetAddress.getByName(splitted(0)))
        val rangeEnd: Long = ipToLong(InetAddress.getByName(splitted(1)))
        val networkName = splitted(2)

        (rangeBegin, rangeEnd, networkName) :: acc
      })

    println("ranges splitted")

    val path: Path = Paths.get(getPath("/"))

    val output = Paths.get(path.toString, "output.tsv")
    if (Files.notExists(output)) {
      Files.createFile(output)
    }

    /*
    //    SimpleARM(Files.newBufferedWriter(output))(writer => {
    //      for {
    //        (userId, ips) <- transactions
    //        //        (network, r) <- ranges
    //        //        (start, end) <- r
    //        ip <- ips
    //        network ← search(ranges, ipToLong(InetAddress.getByName(ip)))
    //      } {
    //        writer.write(s"$userId\t$network")
    //        writer.newLine()
    //      }
    //    })
     */

    //    val by = ranges.groupBy(_._3)

    print()

    var i = 0
    var j = 0

    var iStrings =ArrayBuffer[String]()
    var jStrings = ArrayBuffer[String]()

    println(s"transactions size: ${transactions.size}")
    println(s"ranges size: ${ranges.size}")

    SimpleARM(Files.newBufferedWriter(output))(writer ⇒ {
      for {
        (userId, ips) <- transactions
        (start, end, network) <- ranges
        ip <- ips
      } {
//        if(isValidRange(start, end, ip)) {
        val range: Boolean = isValidRange(start, end, ip)
//          writer.write(s"$start $end $ip $range \n")
//        }

        if (range) {
          i += 1
          iStrings += s"$start $end $ip"
        }
      }
    })

    SimpleARM(Files.newBufferedWriter(output))(writer ⇒ {
      for {
        (userId, ips) <- transactions
        (start, end, network) <- ranges
        ip <- ips
      } {
        //        if(isValidRange(start, end, ip)) {
        val range: Boolean = isValidRange(start, end, ip)
        //          writer.write(s"$start $end $ip $range \n")
        //        }

        if (range) {
          j += 1
          jStrings += s"$start $end $ip"
        }
      }
    })

    println(i)
    println(j)

    println(iStrings)
    println(jStrings)

  }

  def search[E](rBTree: RBTree[E], key: Long): List[E] = {

    def helper(subTree: RBTree[E], key: Long, acc: List[E]): List[E] = {
      if (subTree.isEmpty) acc
      else {
        val i = subTree.value
        val keyHigher: Boolean = key >= i.begin
        val keyLower: Boolean = key <= i.end
        val overlaps: Boolean = keyHigher && keyLower
        val _acc: List[E] = if (overlaps) i.data :: acc else acc
        val helper1: List[E] = helper(subTree.left, key, _acc)
        if (keyHigher) {
          helper(subTree.right, key, helper1)
        } else {
          helper1
        }
      }
    }
    helper(rBTree, key, List())
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
      println("closed")
      c.close()
    }
  }
}
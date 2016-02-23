package com.adform.task

import java.io._
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import com.adform.task.scala_rb_tree_for_intervals.{Interval, Tree}

import scala.io.Source
import scala.language.{higherKinds, implicitConversions}

object Main extends {

  val transactionsPath = "/transactions.tsv"
  val rangesPath: String = "/ranges.tsv"
  val outPath = "output.tsv"

  def main(args: Array[String]) {

    val transactions: Map[String, List[String]] = parseTransactions

    val ranges: Tree[String, Long] = parseRanges

    val output = Paths.get(Paths.get(getPath("/")).toString, outPath)
    if (Files.notExists(output)) {
      Files.createFile(output)
    }

    SimpleARM(Files.newBufferedWriter(output))(writer => {
      for {
        (userId, ips) <- transactions
        ip <- ips
        network ← ranges.search(ip)
      } {
        writer.write(s"$userId\t$network\n")
      }
    })
  }

  private def getPath(path: String) = getClass.getResource(path).toURI

  private def readResource(path: String): Source = Source.fromFile(getPath(path))

  private def parseRanges: Tree[String, Long] = {
    val rangesLines = readResource(rangesPath).getLines()

    rangesLines.map(_.split("-|\t"))
      .foldLeft(Tree[String, Long]())((tree, splitted) => {

        val rangeBegin = splitted(0)
        val rangeEnd = splitted(1)
        val networkName = splitted(2)

        tree.add(Interval(rangeBegin, rangeEnd, networkName))
      })
  }

  private def parseTransactions: Map[String, List[String]] = {
    val transactionsLines = readResource(transactionsPath).getLines()

    transactionsLines.map(_.split("\t"))
      .foldLeft(Map[String, List[String]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap + (userId -> (ip :: accMap(userId)))
      })
  }

  implicit def ipToLong(ip: String): Long = {
    val octets: Array[Byte] = InetAddress.getByName(ip).getAddress
    var result: Long = 0
    for (octet <- octets) {
      result <<= 8
      result |= octet & 0xff
    }
    result
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
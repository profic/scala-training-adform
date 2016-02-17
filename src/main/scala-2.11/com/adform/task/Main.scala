package com.adform.task

import java.io.Closeable
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable
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
    val transactionsLines: Iterator[String] = transactionsSource.getLines()

    val transactions: Map[String, List[String]] = transactionsLines.map(_.split("\t"))
      .foldLeft(mutable.Map[String, List[String]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap(userId) = ip :: accMap(userId)
        accMap
      }).toMap

    val rangesSource = readResource("/ranges.tsv")
    val rangesLines: Iterator[String] = rangesSource.getLines()

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(mutable.Map[String, List[(String, String)]]().withDefaultValue(Nil))((accMap, splitted) => {
        val rangeBegin: String = splitted(0)
        val rangeEnd: String = splitted(1)
        val networkName = splitted(2)

        accMap(networkName) = (rangeBegin, rangeEnd) :: accMap(networkName)
        accMap
      }).toMap

    println("ranges splitted")

    val path: Path = Paths.get(getPath("/"))

    val output = Paths.get(path.toString, "output.tsv")
    if (Files.notExists(output)) {
      Files.createFile(output)
    }

    SimpleARM(Files.newBufferedWriter(output))(writer => {
      for {
        (userId, ips) <- transactions
        (network, r) <- ranges
        (start, end) <- r
        ip <- ips if isValidRange(start, end, ip)
      } {
        writer.write(s"$userId\t$network")
        writer.newLine()
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

object SimpleARM {
  def apply[T <: Closeable, Q](c: T)(f: (T) => Q): Q = {
    try {
      f(c)
    } finally {
      c.close()
    }
  }
}
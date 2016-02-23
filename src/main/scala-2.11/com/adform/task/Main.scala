package com.adform.task

import java.io._
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import com.adform.task.scala_rb_tree_for_intervals.{Interval, Tree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.language.{higherKinds, implicitConversions}

/**
  * Created by vladislav.molchanov on 16.02.2016.
  */
object Main extends {

  val transactionsPath = "/transactions.tsv"
  val rangesPath: String = "/ranges.tsv"
  val outPath = "output.tsv"

  def main(args: Array[String]) {

    def getPath(path: String) = getClass.getResource(path).toURI

    def readResource(path: String): Source = Source.fromFile(getPath(path))

    val transactionsSource = readResource(transactionsPath)
    val transactionsLines = transactionsSource.getLines().toList

    val transactions: Map[String, List[String]] = transactionsLines.map(_.split("\t"))
      .foldLeft(mutable.Map[String, List[String]]().withDefaultValue(Nil))((accMap, splitted) => {
        val userId = splitted(0)
        val ip = splitted(1)
        accMap(userId) = ip :: accMap(userId)
        accMap
      }).toMap

    val rangesSource = readResource(rangesPath)
    val rangesLines = rangesSource.getLines().toList

    val ranges = rangesLines.map(_.split("-|\t"))
      .foldLeft(Tree[String, Long]())((tree, splitted) => {

        val rangeBegin = splitted(0)
        val rangeEnd = splitted(1)
        val networkName = splitted(2)

        tree.add(Interval(rangeBegin, rangeEnd, networkName))
      })

    val path: Path = Paths.get(getPath("/"))

    val output = Paths.get(path.toString, outPath)
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
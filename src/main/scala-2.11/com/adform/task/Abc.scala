package com.adform.task

import java.nio.file.{Files, Paths}

import scala.collection.immutable.TreeMap
import scala.io.Source

/**
  * Created by vladislav.molchanov on 17.02.2016.
  */
object Abc {

  def main(args: Array[String]) {


    def getPath(path: String) = getClass.getResource(path).toURI
    def readResource(path: String): Source = Source.fromFile(getPath(path))
    val rangesSource = readResource("/ranges.tsv")

    val ranges: List[String] = rangesSource.getLines().toList.map(_.split("\t")(0))

    val writer = Files.newBufferedWriter(Paths.get(getPath("/ranges.tsv")))

    var cnt = 1
    for (i <- 1 to (1000000 / ranges.size)) {
      ranges.foreach(r => {
        writer.write(s"$r\tNetwork$cnt")
        writer.newLine()
        cnt += 1
      })
      writer.flush()
    }
    writer.close()

  }

}

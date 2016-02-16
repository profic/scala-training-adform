package com.adform.task

import scala.collection.mutable
import scala.io.Source

/**
  * Created by vladislav.molchanov on 16.02.2016.
  */
object Main extends {

  def main(args: Array[String]) {
    val path = getClass.getResource("/ranges.tsv").toURI
    val ranges = Source.fromFile(path)
    val lines: Iterator[String] = ranges.getLines()

    val aggMap = mutable.Map[String, List[String]]().withDefaultValue(Nil)

    val left: mutable.Map[String, List[String]] = lines.map(_.split("\t"))
      .foldLeft(aggMap)((map, splitted) => {
        val userId = splitted(0)
        val ip = splitted(0)
        map(userId) = ip :: aggMap(userId)
        map
      })

    
  }
}

package com.adform.task.scala_rb_tree_for_intervals

import java.net.InetAddress
import java.nio.file.{Path, Paths, Files}

import com.adform.task.SimpleARM

import scala.collection.mutable.ArrayBuffer
import scala.util.{Random ⇒ r}

/**
  * Created by Vlad on 21.02.2016.
  */
object RandomIpGenerator {

  def main(args: Array[String]) {

    val path: Path = Paths.get(getClass.getResource("/output.tsv").toURI)

    def generate: List[Int] = r.nextInt(256) :: r.nextInt(256) :: r.nextInt(256) :: r.nextInt(256) :: Nil

    val buf = new ArrayBuffer[String]()

    for (i ← 1 to 1000000) {
      val begin = generate
      val b0: Int = begin.head
      val b1: Int = begin(1)
      val b2: Int = begin(2)
      val b3: Int = begin(3)

      def getEnd: List[Int] = {
        if (b1 < 255) {
          val end = r.nextInt(256)
          if (end <= b1) getEnd
          else b0 :: end :: b2 :: b3 :: Nil
        } else if (b2 < 255) {
          val end = r.nextInt(256)
          if (end <= b2) getEnd
          else b0 :: b1 :: end :: b3 :: Nil
        } else {
          val end = r.nextInt(256)
          if (end <= b3) getEnd
          else b0 :: b1 :: b2 :: end :: Nil
        }
      }
      buf += s"${begin.mkString(".")}-${getEnd.mkString(".")}\tNetwork$i\n"
    }

    SimpleARM(Files.newBufferedWriter(path))(w ⇒ {buf.foreach(w.write)})


    //    for (i ← 1 to 10000) {
    //      val begin = generate
    //
    //      val _begin = ipToLong(InetAddress.getByName(generate.productPrefix))
    //
    //      def genEnd: (Long, String) = {
    //        val end: String = generate
    //        val _end = ipToLong(InetAddress.getByName(end))
    //        if (_begin >= _end) genEnd
    //        else (_end, end)
    //      }
    //      buf += s"$begin-${genEnd._2}\tNetwork$i\n"
    //    }


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

}

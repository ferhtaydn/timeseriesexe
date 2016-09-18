package com.ferhtaydn

import java.io.FileWriter
import java.nio.file.{ Files, Paths }

import scala.util.Try

package object timeseriesexe {

  def read(path: String): Option[Array[String]] = Try(scala.io.Source.fromFile(path)).toOption match {
    case Some(bufferedSource) ⇒ Some(bufferedSource.getLines().toArray)
    case None                 ⇒ None
  }

  def append(path: String, txt: String): Unit = {
    val fw = new FileWriter(path, true)
    fw.write(txt)
    fw.append("\n")
    fw.close()
  }

  def remove(file: String): Boolean = Files.deleteIfExists(Paths.get(file))

}

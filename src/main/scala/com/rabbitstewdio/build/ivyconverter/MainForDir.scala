package com.rabbitstewdio.build.ivyconverter

import java.io.File

import org.clapper.argot.ArgotConverters._
import org.clapper.argot.{ArgotException, ArgotParser}

import scala.collection.mutable.ArrayBuffer

object MainForDir {
  def main(args: Array[String]) = {
    val parser = new ArgotParser("ivyconverter")
    val config = parser.option[String](List("c", "config"), "config.conf", "A configuration file")
    val dirs = parser.multiParameter[String]("directories", "target directories", optional = true)

    try {

      parser.parse(args)

      val dirsDefault = if (dirs.value.isEmpty) {
        Seq(".")
      }
      else {
        dirs.value
      }

      val dirMapped = dirsDefault.map(findSubDirs)

      val argBuilder = ArrayBuffer[String]()
      if (config.value.isDefined) {
        argBuilder.append("--config")
        argBuilder.append(config.value.get)
      }
      argBuilder.appendAll(dirMapped.flatten.map(f => f.getPath))
      Main.main(argBuilder.toArray)
    }
    catch {
      case e: ArgotException => parser.usage()
    }
  }

  def findSubDirs(dir: String): List[File] = {
    val path = new File(dir)
    val projectDirs = path.listFiles().filter(f => {
      if (!f.isDirectory) {
        false
      }
      else {
        val c = new File(f, "build.xml")
        val exists = c.exists()
        exists
      }
    }).toList
    projectDirs
  }
}

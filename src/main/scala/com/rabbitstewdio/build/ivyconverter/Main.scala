package com.rabbitstewdio.build.ivyconverter

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.{ArgotException, ArgotParser}

object Main {

  private def loadConfig(file: Option[File]): Config = {
    if (file.isDefined) {
      val config = ConfigFactory.load()
      val conf = ConfigFactory.parseFileAnySyntax(file.get)
      ConfigFactory.load(conf)
    }
    else {
      ConfigFactory.load()
    }
  }

  def main(args: Array[String]): Unit = {
    val parser = new ArgotParser("ivyconverter")
    val config = parser.option[String](List("c", "config"), "config.conf", "A configuration file")
    val dirs = parser.multiParameter[String]("directories", "target directories", optional = false)
    val print = parser.flag[Boolean](List("p", "print"), "print generated POM")

    try {

      parser.parse(args)

      val configObj = loadConfig(config.value.map(f => new File(f)))

      val projectDefs: List[ProjectDef] = validateArgDirectories(dirs.value, configObj)
      projectDefs.map(IvyParser.loadDependencyInformation).
              map(ProjectNormalizer.normalizeDependencyParameter).
              map(_.translateDependenciesToMaven).
              map(PomBuilder.generateMavenPom(print.value.getOrElse(false)))
    }
    catch {
      case e: ArgotException => parser.usage()
    }
  }

  private def validateArgDirectories(args: Seq[String], config: Config): List[ProjectDef] = {
    val dirs = args.foldLeft(List[ProjectDef]())((l, arg) => {
      val candidate = new File(arg)
      if (candidate.isDirectory && candidate.exists()) {
        l :+ ProjectDef(candidate, config, "build.xml")
      }
      else {
        println(s"Directory does not exist: $candidate")
        l
      }
    })
    dirs
  }


}

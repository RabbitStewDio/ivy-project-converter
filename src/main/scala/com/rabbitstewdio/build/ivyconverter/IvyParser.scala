package com.rabbitstewdio.build.ivyconverter

import java.io.File
import java.util

import com.typesafe.config.{ConfigException, Config}
import org.apache.tools.ant.{ProjectHelper, Project}

import scala.collection.JavaConverters._
import scala.xml.{NodeSeq, Node}

object IvyParser {

  def loadDependencyInformation(pd: ProjectDef): ParsedProject = {
    println(s"Processing ${pd.dir}")
    val project = new Project()
    val buildFile = new File(pd.dir, pd.antFile)
    project.init()
    ProjectHelper.configureProject(project, buildFile)

    val raw: util.Hashtable[String, AnyRef] = project.getProperties
    val scalaMap = raw.asInstanceOf[util.Map[String, AnyRef]].asScala
    val properties = scalaMap.mapValues(v => String.valueOf(v)).toMap
    val x = properties("ivy.artifact.group")

    val ivyFileRef = properties.getOrElse("ivyfile", "ivy.xml")
    val ivyFile = new File(buildFile.getParentFile, ivyFileRef)
    val parsed = scala.xml.XML.loadFile(ivyFile)

    val seq: NodeSeq = parsed \\ "dependencies" \ "@defaultconf"
    val defaultConf = seq.text
    val deps = (parsed \\ "dependency").map(parseDependencies(defaultConf, pd.config))
    ParsedProject(pd.dir, pd.config, properties, deps)
  }

  private def parseExclusion(defaultConf: String)(n: Node): ProjectExclusion = {
    val attrs = n.attributes.asAttrMap
    val org = attrs.get("org")
    val module = attrs.get("module")
    if (module != None) {
      ProjectExclusion(org, module)
    }
    else {
      val name = attrs.get("name")
      ProjectExclusion(org, name)
    }
  }

  private def parseDependencies(defaultConf: String, config: Config)(n: Node): ProjectDependency = {
    val excludes = (n \\ "exclude").map(parseExclusion(defaultConf)).toList
    val packaging = (n \\ "artifact" \ "@type").textOption.getOrElse("jar")
    val attrs = n.attributes.asAttrMap
    val s = attrs.getOrElse("conf", defaultConf)
    val scope = parseScope(s, config)
    val transitive = attrs.getOrElse("transitive", "true") == "true"
    val rev = attrs("rev")
    val org = attrs("org")
    val name = attrs("name")
    val classifier = attrs.get("m:classifer")
    ProjectDependency(org, name, rev, transitive, scope, classifier, packaging, excludes)
  }

  private def parseScope(c: String, config: Config): String = {
    val conf = c.split("->").headOption.getOrElse("compile")
    if (conf.startsWith("default")) {
      "compile"
    }
    else {
      conf match {
        case "" => "compile"
        case "test" => "test"
        case "runtime" => "runtime"
        case "provided" => "provided"
        case _ =>
          try {
            config.getString("ivy-mapping." + conf)
          }
          catch {
            case e: ConfigException => conf
          }
      }
    }
  }

}

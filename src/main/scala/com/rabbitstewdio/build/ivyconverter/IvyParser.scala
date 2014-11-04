package com.rabbitstewdio.build.ivyconverter

import java.io.File
import java.util

import org.apache.tools.ant.{ProjectHelper, Project}

import scala.collection.JavaConverters._
import scala.xml.Node

object IvyParser {

  def loadDependencyInformation(pd: ProjectDef): ParsedProject = {
    println(s"Processing ${pd.dir}")
    val project = new Project()
    val buildFile = new File(pd.dir, pd.antFile)
    project.init()
    ProjectHelper.configureProject(project, buildFile)

    val scalaMap = project.getProperties.asInstanceOf[util.Map[String, AnyRef]].asScala
    val properties = scalaMap.mapValues(v => String.valueOf(v)).toMap

    val ivyFileRef = properties.getOrElse("ivyfile", "ivy.xml")
    val ivyFile = new File(buildFile.getParentFile, ivyFileRef)
    val parsed = scala.xml.XML.loadFile(ivyFile)

    val defaultConf = (parsed \\ "dependencies/@defaultconf").text
    val deps = (parsed \\ "dependency").map(parseDependencies(defaultConf))
    ParsedProject(pd.dir, pd.config, properties, deps)
  }

  private def parseExclusion(defaultConf: String)(n: Node): ProjectExclusion = {
    val attrs = n.attributes.asAttrMap
    val org = attrs.get("org")
    val name = attrs.get("module")
    ProjectExclusion(org, name)
  }

  private def parseDependencies(defaultConf: String)(n: Node): ProjectDependency = {
    val excludes = (n \\ "exclude").map(parseExclusion(defaultConf)).toList
    val attrs = n.attributes.asAttrMap
    val scope = parseScope(attrs.getOrElse("conf", defaultConf))
    val transitive = attrs.getOrElse("transitive", "true") == "true"
    val rev = attrs("rev")
    val org = attrs("org")
    val name = attrs("name")
    val classifier = attrs.get("m:classifer")
    ProjectDependency(org, name, rev, transitive, scope, classifier, excludes)
  }

  private def parseScope(c: String) = {
    val conf = c.split("->").headOption.getOrElse("compile")
    if (conf.startsWith("default")) {
      "compile"
    }
    else {
      conf match {
        case "test" => "test"
        case "runtime" => "runtime"
        case "provided" => "provided"
        case _ => conf
      }
    }
  }

}

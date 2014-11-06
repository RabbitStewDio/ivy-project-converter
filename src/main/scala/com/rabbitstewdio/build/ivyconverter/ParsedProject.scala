package com.rabbitstewdio.build.ivyconverter

import java.io.File
import java.util.Properties

import com.typesafe.config.Config

import scala.collection.JavaConverters._

case class ParsedProject(dir: File,
                         config: Config,
                         properties: Map[String, String],
                         deps: Seq[ProjectDependency]) {
  def resolve(variable: String) = {
    properties.getOrElse(variable, variable)
  }

  def resolveGroup(variable: String) = {
    if (variable == "ivy.artifact.group") {
      "${" + variable + "}"
    }
    else {
      resolve(variable)
    }
  }

  private def exclusionRule(config: Config)(k: String): Boolean = {
    val mappings = new ConfigWrapper(config, "mapping.")
    val includes = config.getStringList("includes").asScala
    val excludes = config.getStringList("excludes").asScala

    if (includes.exists(k.matches)) {
      true
    }
    else if (excludes.exists(k.matches)) {
      false
    }
    else if (mappings.contains(k)) {
      false
    }
    else {
      true
    }
  }

  def filterProjectProperties: Properties = {
    val mappings = new ConfigWrapper(config, "mapping.")

    def mapKeys(t: (String, String)) = {
      val key = mappings.getOrKey(t._1)
      (key, t._2)
    }

    val filteredByKey = properties.map(mapKeys).filterKeys(exclusionRule(config))
    filteredByKey.foldLeft(new Properties())((p, tuple) => {
      p.setProperty(tuple._1, tuple._2)
      p
    })
  }

  def queryRelativeDirectory(key: String) = {
    properties.get(key).fold("${" + key + "}")(dir => {
      val basedir = properties("basedir") + "/"
      "${project.basedir}/" + dir.substring(basedir.length)
    })
  }


  def translateDependenciesToMaven: ParsedProject = {
    val mappings = new ConfigWrapper(config, "mapping.")

    def mapProperty(v: String): String = {

      val regexp = ".*\\$\\{(.*)\\}.*".r
      val replacement = regexp.replaceAllIn(v, (m) => {
        val s = mappings.getOrKey(m.group(1))
        "\\$\\{" + s + "\\}"
      })
      replacement
    }

    def mapExclusion(e: ProjectExclusion): ProjectExclusion = {
      val name = e.name.map(mapProperty)
      val org = e.org.map(mapProperty)
      ProjectExclusion(org, name)
    }

    def mapDep(d: ProjectDependency): ProjectDependency = {
      val exclusions = d.exclusions.map(mapExclusion)
      val org = mapProperty(d.org)
      val name = mapProperty(d.name)
      val rev = mapProperty(d.rev)
      d.copy(org = org, name = name, rev = rev, exclusions = exclusions)
    }

    copy(deps = this.deps.map(mapDep))
  }

}

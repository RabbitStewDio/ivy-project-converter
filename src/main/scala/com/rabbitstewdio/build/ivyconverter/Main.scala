package com.rabbitstewdio.build.ivyconverter

import java.io.{File, FileOutputStream}
import java.util.Properties

import com.typesafe.config.{Config, ConfigException, ConfigFactory}
import org.apache.maven.model.io.xpp3.MavenXpp3Writer
import org.apache.maven.model.{Build, Dependency, Exclusion, Model, Parent, Plugin, PluginExecution, Resource}
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.{ArgotException, ArgotParser}
import org.codehaus.plexus.util.xml.Xpp3Dom

import scala.collection.JavaConverters._

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

  private class ConfigWrapper(val config: Config, prefix: String) {
    def getOrKey(k: String) = {
      try {
        config.getString(prefix + k)
      }
      catch {
        case e: ConfigException => k
      }
    }

    def contains(k: String) = {
      try {
        config.getValue(prefix + k)
        true
      }
      catch {
        case e: ConfigException => false
      }
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
      projectDefs.map(IvyParser.loadDependencyInformation).map(translateDependenciesToMaven).map(generateMavenPom(print.value.getOrElse(false)))
    }
    catch {
      case e: ArgotException => parser.usage()
    }
  }

  private def translateDependenciesToMaven(project: ParsedProject): ParsedProject = {
    val mappings = new ConfigWrapper(project.config, "mapping.")

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
      val org = mapProperty(e.org)
      ProjectExclusion(org, name)
    }

    def mapDep(d: ProjectDependency): ProjectDependency = {
      val exclusions = d.exclusions.map(mapExclusion)
      val org = mapProperty(d.org)
      val name = mapProperty(d.name)
      val rev = mapProperty(d.rev)
      d.copy(org = org, name = name, rev = rev)
    }

    val deps = project.deps.map(mapDep)
    project.copy(deps = deps)
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


  def filterProjectProperties(project: ParsedProject): Properties = {
    val mappings = new ConfigWrapper(project.config, "mapping.")

    def mapKeys(t: (String, String)) = {
      val key = mappings.getOrKey(t._1)
      (key, t._2)
    }

    val filteredByKey = project.properties.map(mapKeys).filterKeys(exclusionRule(project.config))
    filteredByKey.foldLeft(new Properties())((p, tuple) => {
      p.setProperty(tuple._1, tuple._2)
      p
    })
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

  def toMaven(d: ProjectDependency) = {
    val md = new Dependency()
    md.setArtifactId(d.name)
    md.setGroupId(d.org)
    if (d.scope != "compile") {
      md.setScope(d.scope)
    }
    md.setVersion(d.rev)
    d.classifier.foreach(c => md.setClassifier(c))
    if (d.transitive) {
      d.exclusions.foreach(d => md.addExclusion(toMavenExclusion(d)))
    }
    else {
      val ex = new Exclusion
      ex.setGroupId("*")
      ex.setArtifactId("*")
      md.addExclusion(ex)
    }
    md
  }

  def toMavenExclusion(d: ProjectExclusion) = {
    val me = new Exclusion
    me.setArtifactId(d.name.getOrElse("*"))
    me.setGroupId(d.org)
    me
  }

  def createBuild(project: ParsedProject): Option[Build] = {

    val sourceDir = queryRelativeDirectory(project, "src.dir")
    val testSourceDir = queryRelativeDirectory(project, "testsrc.dir")

    val build = new Build
    var buildModified = false

    if (sourceDir != ("${project.basedir}/" + project.config.getString("properties.sourceDir"))) {
      build.setSourceDirectory(sourceDir)

      val res = new Resource()
      res.setDirectory(sourceDir)
      res.addExclude("**/*.java")
      build.addResource(res)

      buildModified = true
    }

    if (testSourceDir != ("${project.basedir}/" + project.config.getString("properties.testSourceDir"))) {
      build.setTestSourceDirectory(testSourceDir)

      val testRes = new Resource()
      testRes.setDirectory(testSourceDir)
      testRes.addExclude("**/*.java")
      build.addTestResource(testRes)

      buildModified = true
    }

    if (project.properties.getOrElse("tests.publish", "false") == "true") {
      val exec = new PluginExecution
      exec.addGoal("test-jar")

      val publishTest = new Plugin
      publishTest.setArtifactId("maven-jar-plugin")
      publishTest.setGroupId("org.apache.maven.plugins")
      publishTest.setVersion("2.2")
      publishTest.addExecution(exec)

      build.addPlugin(publishTest)

      buildModified = true
    }

    val plugins = project.deps.filterNot(notPlugin)
    if (!plugins.isEmpty) {

      val items = new Xpp3Dom("artifactItems")

      val processResources = new PluginExecution
      processResources.addGoal("copy")
      processResources.setPhase("process-test")
      processResources.setConfiguration()

      val dependencies = new Plugin
      dependencies.setArtifactId("maven-dependency-plugin")
      dependencies.setGroupId("org.apache.maven.plugins")
      dependencies.setVersion("2.8")
    }

    if (buildModified) {
      Some(build)
    }
    else {
      None
    }
  }

  private def queryRelativeDirectory(project: ParsedProject, p: String) = {
    val dir = project.properties(p)
    val basedir = project.properties("basedir") + "/"
    "${project.basedir}/" + dir.substring(basedir.length)
  }

  private def notPlugin(p: ProjectDependency) = {
    p.scope match {
      case "provided" => true
      case "runtime" => true
      case "compile" => true
      case "test" => true
      case _ => false
    }
  }

  private def generateMavenPom(print: Boolean)(proj: ParsedProject) = {
    val model = new Model()
    model.setModelVersion("4.0.0")
    model.setParent(createParent(proj.config))
    model.setArtifactId(proj.properties("ivy.artifact.id"))
    model.setPackaging(proj.config.getString("properties.packaging"))
    model.setDescription(proj.properties.getOrElse("imple.title", null))
    model.setProperties(filterProjectProperties(proj))

    val group = proj.properties("ivy.artifact.group")
    if (group != model.getParent.getGroupId) {
      model.setGroupId(group)
    }

    val version = proj.properties("project.revision")
    if (version != model.getParent.getVersion) {
      model.setVersion(version)
    }

    proj.deps.filter(notPlugin).map(toMaven).foreach(d => model.addDependency(d))
    createBuild(proj).foreach(model.setBuild)

    if (print) {
      new MavenXpp3Writer().write(System.out, model)
    }
    else {
      val targetFile = new File(proj.dir, "pom.xml")
      val fout = new FileOutputStream(targetFile)
      try {
        new MavenXpp3Writer().write(fout, model)
      }
      finally {
        fout.close()
      }
    }
    true
  }

  private def createParent(config: Config) = {
    val group = config.getString("properties.parentGroupId")
    val artifact = config.getString("properties.parentArtifactId")
    val version = config.getString("properties.parentVersion")
    val path = config.getString("properties.parentPath")
    val parent = new Parent()
    parent.setArtifactId(artifact)
    parent.setGroupId(group)
    parent.setRelativePath(path)
    parent.setVersion(version)
    parent
  }

}

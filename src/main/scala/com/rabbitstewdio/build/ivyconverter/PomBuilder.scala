package com.rabbitstewdio.build.ivyconverter

import java.io.{FileOutputStream, File}

import com.typesafe.config.{Config, ConfigObject}
import org.apache.maven.model.io.xpp3.MavenXpp3Writer
import org.apache.maven.model.{Parent, Model, Plugin, Resource, Build, PluginExecution}
import org.codehaus.plexus.util.xml.Xpp3Dom
import scala.collection.JavaConverters._

object PomBuilder {

  def createResolve(dir: String, scope: String): PluginExecution = {
    val config = new Xpp3Dom("configuration")
    config.addChild(createText("outputDirectory", dir))
    config.addChild(createText("includeScope", scope))

    val processResources = new PluginExecution
    processResources.setId("resolve-" + scope)
    processResources.addGoal("copy-dependencies")
    processResources.setPhase("prepare-package")
    processResources.setConfiguration(config)
    processResources

  }

  def createBuild(project: ParsedProject): Option[Build] = {

    val sourceDir = project.queryRelativeDirectory("src.dir")
    val testSourceDir = project.queryRelativeDirectory("testsrc.dir")

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

    val plugins = project.deps.filterNot(_.notPlugin)
    if (plugins.nonEmpty) {

      val dependencies = new Plugin
      dependencies.setArtifactId("maven-dependency-plugin")
      dependencies.setGroupId("org.apache.maven.plugins")
      dependencies.setVersion("2.8")

      val configObject: ConfigObject = project.config.getObject("assembly.copy")
      configObject.keySet().asScala.foreach(scope => {
        val dirRef = configObject.toConfig.getString(scope)
        val dir = project.queryRelativeDirectory(dirRef)
        dependencies.addExecution(createResolve(dir, scope))
      })

      val distinctScopes = plugins.map(p => p.scope).distinct
      distinctScopes.map(processScope(plugins, project)).foreach(dependencies.addExecution)

      build.addPlugin(dependencies)
    }

    if (buildModified) {
      Some(build)
    }
    else {
      None
    }
  }

  def projectDepdenencyToXppDom(p: ProjectDependency): Xpp3Dom = {
    val item = new Xpp3Dom("artifactItem")
    item.addChild(createText("groupId", p.org))
    item.addChild(createText("artifactId", p.name))
    item.addChild(createText("version", p.rev))
    item.addChild(createText("type", p.packaging))
    item
  }

  def createText(node: String, text: String): Xpp3Dom = {
    val dom = new Xpp3Dom(node)
    dom.setValue(text)
    dom
  }

  def processScope(plugins: Seq[ProjectDependency], pd: ParsedProject)(s: String) = {

    val pluginsForScope = plugins.withFilter(p => p.scope == s)

    val items = new Xpp3Dom("artifactItems")
    pluginsForScope.map(projectDepdenencyToXppDom).foreach(items.addChild)

    val conf = new ConfigWrapper(pd.config, "")
    val targetDir = if (conf.contains(s"assembly.dirs.$s")) {
      "${project.basedir}/" + pd.config.getString(s"assembly.dirs.$s")
    }
    else {
      pd.queryRelativeDirectory(pd.config.getString(s"assembly.refs.$s"))
    }

    val config = new Xpp3Dom("configuration")
    config.addChild(createText("outputDirectory", targetDir))
    config.addChild(items)

    val processResources = new PluginExecution
    processResources.setId("copy-scope-" + s)
    processResources.addGoal("copy")
    processResources.setPhase("prepare-package")
    processResources.setConfiguration(config)
    processResources
  }


  def generateMavenPom(print: Boolean)(proj: ParsedProject) = {

    val model = new Model()
    model.setModelVersion("4.0.0")
    model.setParent(createParent(proj.config))
    model.setArtifactId(proj.properties("ivy.artifact.id"))
    model.setPackaging(proj.config.getString("properties.packaging"))
    model.setDescription(proj.properties.getOrElse("imple.title", null))
    model.setProperties(proj.filterProjectProperties)

    val group = proj.properties("ivy.artifact.group")
    if (group != model.getParent.getGroupId) {
      model.setGroupId(group)
    }

    val version = proj.properties("project.revision")
    if (version != model.getParent.getVersion) {
      model.setVersion(version)
    }

    proj.deps.filter(_.notPlugin).map(_.toMaven).foreach(d => model.addDependency(d))
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

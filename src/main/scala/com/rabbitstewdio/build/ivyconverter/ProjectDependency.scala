package com.rabbitstewdio.build.ivyconverter

import org.apache.maven.model.{Exclusion, Dependency}

case class ProjectDependency(org: String,
                             name: String,
                             rev: String,
                             transitive: Boolean,
                             scope: String,
                             classifier: Option[String],
                             packaging: String,
                             exclusions: List[ProjectExclusion]) {
  def notPlugin = {
    scope match {
      case "provided" => true
      case "runtime" => true
      case "compile" => true
      case "test" => true
      case _ => false
    }
  }

    def toMaven = {
    val md = new Dependency()
    md.setArtifactId(name)
    md.setGroupId(org)
    if (scope != "compile") {
      md.setScope(scope)
    }
    md.setVersion(rev)
    classifier.foreach(c => md.setClassifier(c))
    if (transitive) {
      exclusions.foreach(d => md.addExclusion(d.toMavenExclusion))
    }
    else {
      val ex = new Exclusion
      ex.setGroupId("*")
      ex.setArtifactId("*")
      md.addExclusion(ex)
    }
    md
  }

}

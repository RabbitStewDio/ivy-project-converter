package com.rabbitstewdio.build.ivyconverter

import org.apache.maven.model.Exclusion

case class ProjectExclusion(org:Option[String], name: Option[String]) {

  def toMavenExclusion = {
    val me = new Exclusion
    me.setArtifactId(name.getOrElse("*"))
    me.setGroupId(org.getOrElse("*"))
    me
  }

}

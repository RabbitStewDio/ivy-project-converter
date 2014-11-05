package com.rabbitstewdio.build.ivyconverter

import java.text.MessageFormat

object ProjectNormalizer {

  def asVariable(s: String): Option[String] = {
    if (s.startsWith("${") && s.endsWith("}")) {
      Some(s.substring(2, s.length - 1))
    }
    else {
      None
    }
  }

  case class ArtifactRef(org: String, name: String) {}
  case class PropertyMapping(oldName: String, newName: String) {}

  def mapRevision(project: ParsedProject)(m: Map[ArtifactRef, PropertyMapping], p: ProjectDependency) = {

    if (p.rev == "${project.revision}") {
      m.updated(ArtifactRef(p.org, p.name), PropertyMapping("project.revision", "project.revision"))
    }
    else {
      val groupId = asVariable(p.org).fold(p.org)(project.resolve)
      val artifactId = asVariable(p.name).fold(p.name)(project.resolve)

      val pattern = project.config.getString("properties.dependency-pattern")

      asVariable(p.rev).foldLeft(m)((m, v) => {
        val pm = PropertyMapping(v, MessageFormat.format(pattern, groupId, artifactId))
        m.updated(ArtifactRef(p.org, p.name), pm)
      })
    }
  }

  def normalizeDependencyParameter(project: ParsedProject): ParsedProject = {

    val revisionMapping = project.deps.foldLeft(Map[ArtifactRef, PropertyMapping]())(mapRevision(project))

    val revisionMappedDeps = project.deps.map(p => {
      asVariable(p.rev).fold(p)(v => {
        val normalizedRev = revisionMapping(ArtifactRef(p.org, p.name))
        p.copy(rev = "${" + normalizedRev.newName + "}")
      })
    })

    val newDeps =
      if (project.config.getBoolean("properties.resolve-group-id")) {
        revisionMappedDeps.map(p => {
          asVariable(p.org).foldLeft(p)((p, v) => {
            p.copy(org = project.resolveGroup(v))
          })
        })
      }
      else {
        revisionMappedDeps
      }

    val updatedProperties = revisionMapping.values.foldLeft(project.properties)((m, pm) => {
      val mx = m.updated(pm.newName, project.properties(pm.oldName))
      mx - pm.oldName
    }).updated("project.revision", project.properties("project.revision"))

    project.copy(properties = updatedProperties, deps = newDeps)
  }

}

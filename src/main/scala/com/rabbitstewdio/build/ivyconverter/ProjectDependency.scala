package com.rabbitstewdio.build.ivyconverter

case class ProjectDependency(org: String,
                             name: String,
                             rev: String,
                             transiative: Boolean,
                             scope: String,
                             classifier: Option[String],
                             exclusions: List[ProjectExclusion]) {
}

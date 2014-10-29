package com.rabbitstewdio.build.ivyconverter

import java.io.File

import com.typesafe.config.Config

case class ParsedProject(dir: File,
                         config: Config,
                         properties: Map[String,String],
                         deps: Seq[ProjectDependency]) {

}

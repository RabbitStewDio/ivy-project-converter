package com.rabbitstewdio.build.ivyconverter

import java.io.File

import com.typesafe.config.Config

case class ProjectDef(dir: File,
                      config: Config,
                      antFile: String) {

}

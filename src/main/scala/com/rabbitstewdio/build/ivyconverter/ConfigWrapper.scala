package com.rabbitstewdio.build.ivyconverter

import com.typesafe.config.{Config, ConfigException}

class ConfigWrapper(val config: Config, prefix: String) {
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

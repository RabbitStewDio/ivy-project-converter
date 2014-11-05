package com.rabbitstewdio.build

import scala.xml.NodeSeq

package object ivyconverter {
  implicit def enrichNodeSeq(nodeSeq: NodeSeq) = new AnyRef {
    def textOption : Option[String] = {
      val text = nodeSeq.text
      if (text == null || text.length == 0) None else Some(text)
    }
  }
}

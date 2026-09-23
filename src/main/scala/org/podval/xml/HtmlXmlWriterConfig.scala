package org.podval.xml

object HtmlXmlWriterConfig extends XmlWriterConfig(
  preformat = XmlElement.preformat.map(_.localName).toSet,
  rawText = XmlElement.rawText.map(_.localName).toSet,
  stack = XmlElement.stack.map(_.localName).toSet,
  unStack = XmlElement.unStack.map(_.localName).toSet,
  break = XmlElement.break.map(_.localName).toSet,
  selfClose = XmlElement.selfClose.map(_.localName).toSet
)

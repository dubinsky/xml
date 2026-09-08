package org.podval.xml

open class XmlElement(val expanded: XmlExpandedName):
  def this(qName: String) = this(XmlExpandedName.parse(qName))

  def qName: String = expanded.qName

object XmlElement:
  object A extends XmlElement("a")

  object Code extends XmlElement("code")

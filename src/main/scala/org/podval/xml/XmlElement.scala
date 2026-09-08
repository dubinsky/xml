package org.podval.xml

open class XmlElement(val expanded: XmlExpandedName):
  def this(name: String) = this(XmlExpandedName.parse(name))

  def name: String = expanded.qualifiedName

object XmlElement:
  object A extends XmlElement("a")

  object Code extends XmlElement("code")

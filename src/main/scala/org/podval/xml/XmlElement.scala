package org.podval.xml

open class XmlElement(val expanded: XmlExpandedName):
  def this(qName: String) = this(XmlExpandedName.parse(qName))

  def qName: String = expanded.qName

  def localName: String = expanded.localName

object XmlElement:
  object A extends XmlElement("a")

  object Code extends XmlElement("code")

  object P extends XmlElement("p")

  object Head extends XmlElement("head")

  object Body extends XmlElement("body")

  object Title extends XmlElement("title")

  object Div extends XmlElement("div")

  object Span extends XmlElement("span")

  object Ul extends XmlElement("ul")

  object Ol extends XmlElement("ol")

  object Li extends XmlElement("li")

  object Img extends XmlElement("img")

  object Pre extends XmlElement("pre")

  object Table extends XmlElement("table")

  object Tr extends XmlElement("tr")

  object Td extends XmlElement("td")

  object Th extends XmlElement("th")

  object Dl extends XmlElement("dl")

  object Dt extends XmlElement("dt")

  object Dd extends XmlElement("dd")

  object Blockquote extends XmlElement("blockquote")

  object Figure extends XmlElement("figure")

  object Figcaption extends XmlElement("figcaption")

  object Br extends XmlElement("br")

  object Em extends XmlElement("em")

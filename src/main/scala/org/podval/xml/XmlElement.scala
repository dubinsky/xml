package org.podval.xml

open class XmlElement(val name: XmlName):
  def this(qName: String) = this(XmlName.parse(qName))

  def qName: String = name.qName

  def localName: String = name.localName

object XmlElement:
  object A extends XmlElement("a")
  object Blockquote extends XmlElement("blockquote")
  object Body extends XmlElement("body")
  object Br extends XmlElement("br")
  object Code extends XmlElement("code")
  object Dd extends XmlElement("dd")
  object Div extends XmlElement("div")
  object Dl extends XmlElement("dl")
  object Dt extends XmlElement("dt")
  object Em extends XmlElement("em")
  object Figcaption extends XmlElement("figcaption")
  object Figure extends XmlElement("figure")
  object Head extends XmlElement("head")
  object Html extends XmlElement("html")
  object Img extends XmlElement("img")
  object Li extends XmlElement("li")
  object Ol extends XmlElement("ol")
  object P extends XmlElement("p")
  object Pre extends XmlElement("pre")
  object Span extends XmlElement("span")
  object Table extends XmlElement("table")
  object Td extends XmlElement("td")
  object Title extends XmlElement("title")
  object Th extends XmlElement("th")
  object Tr extends XmlElement("tr")
  object Ul extends XmlElement("ul")

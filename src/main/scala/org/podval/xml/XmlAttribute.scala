package org.podval.xml

open class XmlAttribute(val expanded: XmlExpandedName):
  def this(name: String) = this(XmlExpandedName.parse(name, isAttribute = true))

  def name: String = expanded.qualifiedName

object XmlAttribute:
  object Id extends XmlAttribute("id")

  object XmlId extends XmlAttribute(XmlExpandedName(
    "id",
    XmlNamespace.xml.prefix,
    Some(XmlNamespace.xml.uri)
  ))

  // TODO lang?

  object XmlBase extends XmlAttribute(XmlExpandedName(
    "base",
    XmlNamespace.xml.prefix,
    Some(XmlNamespace.xml.uri)
  ))

  object Xmlns extends XmlAttribute(XmlExpandedName(
    "xmlns",
    None,
    Some(XmlNamespace.xmlns.uri)
  )):
    def apply(prefix: String): XmlAttribute =
      XmlAttribute(XmlExpandedName(
        prefix,
        XmlNamespace.xmlns.prefix,
        Some(XmlNamespace.xmlns.uri)
      ))

  object Href extends XmlAttribute("href")

  /** The HTML `class` attribute. */
  object HtmlClass extends XmlAttribute("class")

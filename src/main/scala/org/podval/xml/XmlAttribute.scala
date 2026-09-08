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

  object XmlLang extends XmlAttribute(XmlExpandedName(
    "lang",
    XmlNamespace.xml.prefix,
    Some(XmlNamespace.xml.uri)
  ))

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

  object Src extends XmlAttribute("src")

  /** The HTML `lang` attribute. */
  object Lang extends XmlAttribute("lang")

  /** The HTML `class` attribute. */
  object HtmlClass extends XmlAttribute("class")

  object Title extends XmlAttribute("title")

  object Alt extends XmlAttribute("alt")

  object Target extends XmlAttribute("target")

  object Rel extends XmlAttribute("rel")

  object Role extends XmlAttribute("role")

  object Type extends XmlAttribute("type")

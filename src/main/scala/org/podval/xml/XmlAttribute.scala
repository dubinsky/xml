package org.podval.xml

open class XmlAttribute(val expanded: XmlExpandedName):
  def this(qName: String) = this(XmlExpandedName.parse(qName, isAttribute = true))

  /** Default `xmlns` is unprefixed; `xmlns:foo` is `XmlAttribute("foo", xmlns)`. */
  def this(name: String, namespace: XmlNamespace) = this(XmlExpandedName(
    localName = name,
    prefix = if namespace == XmlNamespace.xmlns && namespace.prefix.contains(name) then None else namespace.prefix,
    namespace = Some(namespace.uri)
  ))

  def qName: String = expanded.qName

object XmlAttribute:
  object Id extends XmlAttribute("id")

  object XmlId extends XmlAttribute("id", XmlNamespace.xml)

  object XmlLang extends XmlAttribute("lang", XmlNamespace.xml)

  object XmlBase extends XmlAttribute("base", XmlNamespace.xml)

  object Xmlns extends XmlAttribute("xmlns", XmlNamespace.xmlns):
    def apply(prefix: String): XmlAttribute = XmlAttribute(prefix, XmlNamespace.xmlns)

  object Href extends XmlAttribute("href")

  object Src extends XmlAttribute("src")

  /** The HTML `lang` attribute. */
  object Lang extends XmlAttribute("lang")

  /** The HTML `class` attribute. */
  object CssClass extends XmlAttribute("class")

  object Title extends XmlAttribute("title")

  object Alt extends XmlAttribute("alt")

  object Target extends XmlAttribute("target")

  object Rel extends XmlAttribute("rel")

  object Role extends XmlAttribute("role")

  object Type extends XmlAttribute("type")

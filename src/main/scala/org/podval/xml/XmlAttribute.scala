package org.podval.xml

// TODO remove aux constructors?
open class XmlAttribute(val expanded: XmlExpandedName):
  def this(qName: String) = this(XmlExpandedName.parse(qName, isAttribute = true))

  /** Default `xmlns` is unprefixed; `xmlns:foo` is `XmlAttribute("foo", xmlns)`. */
  def this(name: String, namespace: XmlNamespace) = this(XmlExpandedName(
    localName = name,
    namespace = Some(
      if namespace == XmlNamespace.xmlns && namespace.prefix.contains(name) then namespace.unprefixed
      else namespace
    )
  ))

  def qName: String = expanded.qName

  def localName: String = expanded.localName

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

  /** HTML `table@frame` / frameset `frame`; TEI tables use the same local name. */
  object Frame extends XmlAttribute("frame")

  object Rel extends XmlAttribute("rel")

  object Role extends XmlAttribute("role")

  object Type extends XmlAttribute("type")

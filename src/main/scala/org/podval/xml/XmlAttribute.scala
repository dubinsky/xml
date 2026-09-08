package org.podval.xml

open class XmlAttribute(val name: XmlName):
  def this(qName: String) = this(XmlName.parse(qName, isAttribute = true))

  /** Default `xmlns` is unprefixed; `xmlns:foo` is `XmlAttribute("foo", xmlns)`. */
  def this(localName: String, namespace: XmlNamespace) = this(XmlName(
    localName = localName,
    namespace = Some(
      if namespace == XmlNamespace.xmlns && namespace.prefix.contains(localName)
      then namespace.unprefixed
      else namespace
    )
  ))

  def qName: String = name.qName

  def localName: String = name.localName

  def matches(other: XmlName): Boolean = other.is(this)

object XmlAttribute:
  object Alt extends XmlAttribute("alt")

  object XmlBase extends XmlAttribute("base", XmlNamespace.xml)

  object CssClass extends XmlAttribute("class")

  object Id extends XmlAttribute("id")

  /** HTML `table@frame` / frameset `frame`; TEI tables use the same local name. */
  object Frame extends XmlAttribute("frame")

  object XmlId extends XmlAttribute("id", XmlNamespace.xml)

  object Href extends XmlAttribute("href")

  object XmlLang extends XmlAttribute("lang", XmlNamespace.xml)

  /** The HTML `lang` attribute. */
  object Lang extends XmlAttribute("lang")

  object Rel extends XmlAttribute("rel")

  object Role extends XmlAttribute("role")
  
  object Src extends XmlAttribute("src")

  object Target extends XmlAttribute("target")

  object Title extends XmlAttribute("title")

  object Type extends XmlAttribute("type")
  
  object Xmlns extends XmlAttribute("xmlns", XmlNamespace.xmlns):
    def apply(prefix: String): XmlAttribute = XmlAttribute(prefix, XmlNamespace.xmlns)

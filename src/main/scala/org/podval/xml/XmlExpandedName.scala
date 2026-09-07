package org.podval.xml

import zio.blocks.schema.Schema

/** Expanded name: local part, optional prefix, optional namespace URI.
  *
  * `xmlns*` stay attributes (declarations for the writer). This is the
  * in-scope name of an element or attribute, including URIs inherited from
  * ancestors. */
final case class XmlExpandedName(
  localName: String,
  prefix: Option[String] = None,
  namespace: Option[String] = None
) derives CanEqual:
  def qualifiedName: String =
    prefix.filter(_.nonEmpty).fold(localName)(p => s"$p:$localName")

object XmlExpandedName:
  given schema: Schema[XmlExpandedName] = Schema.derived

  def parseQualified(name: String): XmlExpandedName =
    val colon: Int = name.indexOf(':')
    if colon <= 0 then XmlExpandedName(name)
    else XmlExpandedName(
      localName = name.substring(colon + 1),
      prefix = Some(name.substring(0, colon))
    )

  def parse(
    name: String,
    attributes: Seq[(String, String)] = Seq.empty,
    isAttribute: Boolean = false,
    existing: Option[XmlExpandedName] = None
  ): XmlExpandedName =
    val parsed: XmlExpandedName = parseQualified(name)
    val namespace: Option[String] =
      XmlNamespace.wellKnown(parsed.prefix, parsed.localName, isAttribute)
        .orElse(xmlnsUri(parsed.prefix, attributes, isAttribute))
        .orElse(existing.filter(_.prefix == parsed.prefix).flatMap(_.namespace))
    parsed.copy(namespace = namespace)

  def attributes(attributes: Seq[(String, String)]): Seq[(XmlExpandedName, String)] =
    attributes.map((name, value) => parse(name, attributes, isAttribute = true) -> value)

  def xmlnsUri(
    prefix: Option[String],
    attributes: Seq[(String, String)],
    isAttribute: Boolean
  ): Option[String] =
    prefix match
      case Some(p) => attributes.collectFirst { case (n, v) if n == s"xmlns:$p" => v }
      case None if !isAttribute => attributes.collectFirst { case (n, v) if n == "xmlns" => v }
      case None => None

  def asPairs(attributes: Seq[(XmlExpandedName, String)]): Seq[(String, String)] =
    attributes.map((name, value) => name.qualifiedName -> value)

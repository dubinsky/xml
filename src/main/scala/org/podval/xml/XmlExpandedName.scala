package org.podval.xml

import zio.blocks.schema.Schema
import zio.blocks.schema.xml.XmlName

/** Expanded name: local part and optional namespace (URI + prefix).
  *
  * `xmlns*` stay attributes (declarations for the writer). This is the
  * in-scope name of an element or attribute, including URIs inherited from
  * ancestors. */
// TODO clean up
final case class XmlExpandedName(
  localName: String,
  namespace: Option[XmlNamespace] = None
) derives CanEqual:
  def prefix: Option[String] = namespace.flatMap(_.prefix)

  def uri: Option[String] = namespace.map(_.uri).filter(_.nonEmpty)

  def qName: String =
    prefix.filter(_.nonEmpty).fold(localName)(p => s"$p:$localName")

  def isDefaultXmlns: Boolean =
    prefix.isEmpty && (XmlNamespace.xmlns.prefix.contains(localName) || uri.contains(XmlNamespace.xmlns.uri))

  def isXmlnsDeclaration: Boolean =
    prefix == XmlNamespace.xmlns.prefix || isDefaultXmlns

  def isXml: Boolean =
    prefix == XmlNamespace.xml.prefix || uri.contains(XmlNamespace.xml.uri)

  /** Same in-scope name: local part and URI, ignoring prefix. */
  def sameAs(other: XmlExpandedName): Boolean =
    localName == other.localName && uri == other.uri

  def matches(expected: String): Boolean =
    val exp: XmlExpandedName = XmlExpandedName.parseQName(expected)
    qName == expected || localName == expected || localName == exp.localName

  def toZio: XmlName = XmlName(
    localName = localName,
    prefix = prefix,
    namespace = uri
  )

object XmlExpandedName:
  given schema: Schema[XmlExpandedName] = Schema.derived

  def fromZio(name: XmlName): XmlExpandedName =
    XmlExpandedName(name.localName, XmlNamespace.of(name.prefix, name.namespace))

  def parseQName(name: String): XmlExpandedName =
    name.span(_ != ':') match
      case (prefix, rest) if prefix.nonEmpty && rest.nonEmpty =>
        XmlExpandedName(localName = rest.drop(1), namespace = XmlNamespace.of(Some(prefix), None))
      case _ => XmlExpandedName(name)

  def parse(
    name: String,
    attributes: Seq[(String, String)] = Seq.empty,
    isAttribute: Boolean = false,
    existing: Option[XmlExpandedName] = None
  ): XmlExpandedName =
    val parsed: XmlExpandedName = parseQName(name)
    bind(parsed, isAttribute, xmlnsUri(parsed.prefix, attributes, isAttribute), existing)

  /** Like `parse`, with xmlns taken from already-expanded attributes (`declaredUri`). */
  def parseDeclared(
    name: String,
    attributes: Seq[(XmlExpandedName, String)],
    isAttribute: Boolean = false,
    existing: Option[XmlExpandedName] = None
  ): XmlExpandedName =
    val parsed: XmlExpandedName = parseQName(name)
    bind(parsed, isAttribute, declaredUri(parsed.prefix, attributes, isAttribute), existing)

  private def bind(
    parsed: XmlExpandedName,
    isAttribute: Boolean,
    declared: Option[String],
    existing: Option[XmlExpandedName]
  ): XmlExpandedName =
    val uri: Option[String] =
      XmlNamespace.wellKnown(parsed.prefix, parsed.localName, isAttribute).map(_.uri)
        .orElse(declared)
        .orElse(existing.filter(_.prefix == parsed.prefix).flatMap(_.uri))
    parsed.copy(namespace = XmlNamespace.of(parsed.prefix, uri))

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

  def declaredUri(
    prefix: Option[String],
    attributes: Seq[(XmlExpandedName, String)],
    isAttribute: Boolean
  ): Option[String] =
    prefix match
      case Some(p) =>
        attributes.collectFirst { case (n, v) if n.isXmlnsDeclaration && n.localName == p => v }
      case None if !isAttribute =>
        attributes.collectFirst { case (n, v) if n.isDefaultXmlns => v }
      case None => None

  def asPairs(attributes: Seq[(XmlExpandedName, String)]): Seq[(String, String)] =
    attributes.map((name, value) => name.qName -> value)

  def xmlnsAttribute(prefix: Option[String], uri: String): (XmlExpandedName, String) =
    prefix.filter(_.nonEmpty) match
      case Some(p) => XmlExpandedName(localName = p, namespace = Some(XmlNamespace.xmlns)) -> uri
      case None => XmlExpandedName(localName = "xmlns", namespace = Some(XmlNamespace.xmlns.unprefixed)) -> uri

  /** `xmlns*` for URIs on `name` and attributes that are not already declared. */
  def xmlnsDeclarations(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)]
  ): Seq[(XmlExpandedName, String)] =
    (name +: attributes.map(_._1)).foldLeft(Seq.empty[(XmlExpandedName, String)]): (acc, n) =>
      n.uri match
        case Some(uri) if !n.isXmlnsDeclaration && !n.isXml =>
          val all: Seq[(XmlExpandedName, String)] = attributes ++ acc
          if declaredUri(n.prefix, all, isAttribute = false).isDefined then acc
          else acc :+ xmlnsAttribute(n.prefix, uri)
        case _ => acc

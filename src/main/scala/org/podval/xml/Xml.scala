package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{XmlName, Xml as XML}

// XML AST for ZIO Blocks XML
given Xml: XmlAst[XML.Element]:
  override type Node = XML
 
  override def text(text: String): Node = XML.Text(text)

  override def cdata(text: String): Node = XML.CData(text)

  override def element(name: String, attributes: Seq[(String, String)], children: Nodes): Element = XML.Element(
    name = xmlName(name, attributes, isAttribute = false),
    attributes = toAttributes(attributes),
    children = Chunk.from(children)
  )

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: XML.Element => Some(element)
      case _ => None

    override def asText: Option[String] = node match
      case XML.Text(value) => Some(value)
      case _ => None

    override def asCData: Option[String] = node match
      case XML.CData(value) => Some(value)
      case _ => None

    override def asAtom: Option[String] = node.asText.orElse(node.asCData)

  extension (element: Element)
    override def getName: String =
      element.name.qualifiedName

    override def rename(name: String): Element =
      element.copy(name = xmlName(name, element.getAttributes, isAttribute = false, existing = Some(element.name)))

    override def getAttributes: Seq[(String, String)] =
      element.attributes.map((xmlName, value) => (xmlName.qualifiedName, value))

    override def setAttributes(attributes: Seq[(String, String)]): Element =
      element.copy(
        name = xmlName(element.getName, attributes, isAttribute = false, existing = Some(element.name)),
        attributes = toAttributes(attributes)
      )

    override def getChildren: Nodes =
      element.children

    override def setChildren(children: Nodes): Element =
      element.copy(children = Chunk.from(children))

  private def toAttributes(attributes: Seq[(String, String)]): Chunk[(XmlName, String)] =
    Chunk.from(attributes).map((name, value) => (xmlName(name, attributes, isAttribute = true), value))

  // ZIO `XmlName.apply(String)` does not split a prefix. Unprefixed attributes
  // do not take the default namespace (https://www.w3.org/TR/xml-names/).
  private def xmlName(
    name: String,
    attributes: Seq[(String, String)],
    isAttribute: Boolean,
    existing: Option[XmlName] = None
  ): XmlName =
    val parsed: XmlName = parseQualified(name)
    val namespace: Option[String] =
      namespaceOf(parsed.prefix, parsed.localName, attributes, isAttribute)
        .orElse(existing.filter(_.prefix == parsed.prefix).flatMap(_.namespace))
    parsed.copy(namespace = namespace)

  private def parseQualified(name: String): XmlName =
    val colon: Int = name.indexOf(':')
    if colon <= 0 then XmlName(name)
    else XmlName(
      localName = name.substring(colon + 1),
      prefix = Some(name.substring(0, colon)),
      namespace = None
    )

  private def namespaceOf(
    prefix: Option[String],
    local: String,
    attributes: Seq[(String, String)],
    isAttribute: Boolean
  ): Option[String] =
    XmlNamespace.wellKnown(prefix, local, isAttribute).orElse:
      prefix match
        case Some(p) => attributes.collectFirst { case (n, v) if n == s"xmlns:$p" => v }
        case None if !isAttribute => attributes.collectFirst { case (n, v) if n == "xmlns" => v }
        case None => None

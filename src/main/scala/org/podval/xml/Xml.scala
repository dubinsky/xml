package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{XmlName, Xml as XML}

// XML AST for ZIO Blocks XML
given Xml: XmlAst[XML.Element]:
  override type Node = XML

  override def text(text: String): Node = XML.Text(text)

  override def cdata(text: String): Node = XML.CData(text)

  override def comment(text: String): Option[Node] = Some(XML.Comment(text))

  override def processingInstruction(target: String, data: String): Option[Node] =
    Some(XML.ProcessingInstruction(target, data))

  override def element(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)],
    children: Nodes
  ): Element = XML.Element(
    name = toZio(name, attributes, isAttribute = false),
    attributes = Chunk.from(attributes).map((attr, value) => (toZio(attr, attributes, isAttribute = true), value)),
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

    override def asComment: Option[String] = node match
      case XML.Comment(value) => Some(value)
      case _ => None

    override def asProcessingInstruction: Option[(String, String)] = node match
      case XML.ProcessingInstruction(target, data) => Some((target, data))
      case _ => None

    override def asAtom: Option[String] = node.asText.orElse(node.asCData)

  extension (element: Element)
    override def getExpandedName: XmlExpandedName =
      fromZio(element.name)

    override def getName: String = element.getExpandedName.qualifiedName

    override def localName: String = element.getExpandedName.localName

    override def getPrefix: Option[String] = element.getExpandedName.prefix

    override def getNamespace: Option[String] = element.getExpandedName.namespace

    override def rename(name: String): Element = renamed(element, name)

    override def getExpandedAttributes: Seq[(XmlExpandedName, String)] =
      element.attributes.map((name, value) => (fromZio(name), value))

    override def getAttributes: Seq[(String, String)] =
      XmlExpandedName.asPairs(element.getExpandedAttributes)

    override def setAttributes(attributes: Seq[(String, String)]): Element =
      withAttributes(element, attributes)

    override def set(attribute: String, value: String): Element =
      withAttribute(element, attribute, value)

    override def set(attribute: XmlAttribute, value: String): Element =
      withAttribute(element, attribute.name, value)

    override def getChildren: Nodes =
      element.children

    override def setChildren(children: Nodes): Element =
      withChildren(element, children)

  private def fromZio(name: XmlName): XmlExpandedName =
    XmlExpandedName(name.localName, name.prefix, name.namespace)

  private def toZio(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)],
    isAttribute: Boolean
  ): XmlName =
    val pairs: Seq[(String, String)] = XmlExpandedName.asPairs(attributes)
    val namespace: Option[String] = name.namespace
      .orElse(XmlNamespace.wellKnown(name.prefix, name.localName, isAttribute))
      .orElse(XmlExpandedName.xmlnsUri(name.prefix, pairs, isAttribute))
    XmlName(
      localName = name.localName,
      prefix = name.prefix,
      namespace = namespace
    )

package org.podval.xml

import scala.annotation.unused
import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.Xml as XML
import zio.blocks.schema.xml.XmlName as ZioXmlName

// XML AST for ZIO Blocks XML.
// `import ZioBlocksXml.given` to convert (`element.to[ZioBlocksXml.Element]`).
object ZioBlocksXml extends XmlAst[XML.Element]:
  given ZioBlocksXml.type = this

  override type Node = XML

  override def text(text: String): Node = XML.Text(text)

  override def cdata(text: String): Node = XML.CData(text)

  override def comment(text: String): Option[Node] = Some(XML.Comment(text))

  override def processingInstruction(target: String, data: String): Option[Node] = Some(XML.ProcessingInstruction(target, data))

  override def element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Nodes
  ): Element = XML.Element(
    name = toZio(name, attributes, isAttribute = false),
    attributes = Chunk.from(attributes).map((attr, value) => (toZio(attr, attributes, isAttribute = true), value)),
    children = Chunk.from(children)
  )

  override def foldNode[A](
    node: Node,
    element: Element => A,
    text: String => A,
    cdata: String => A,
    comment: String => A,
    processingInstruction: (String, String) => A,
    @unused unknown: => A
  ): A = node match
    case value: XML.Element => element(value)
    case XML.Text(value) => text(value)
    case XML.CData(value) => cdata(value)
    case XML.Comment(value) => comment(value)
    case XML.ProcessingInstruction(target, data) => processingInstruction(target, data)

  override def nameOf(element: Element): XmlName = fromZio(element.name)

  override def childrenOf(element: Element): Nodes = element.children

  override def attributesOf(element: Element): Seq[(XmlName, String)] =
    element.attributes.map((name, value) => (fromZio(name), value))

  private def toZio(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    isAttribute: Boolean
  ): ZioXmlName =
    val resolved: Option[String] = name.uri
      .orElse(XmlNamespace.wellKnown(name.prefix, name.localName, isAttribute).map(_.uri))
      .orElse(XmlName.declaredUri(name.prefix, attributes, isAttribute))
    val bound: XmlName = XmlName(name.localName, XmlNamespace.of(name.prefix, resolved))
    ZioXmlName(localName = bound.localName, prefix = bound.prefix, namespace = bound.uri)

  private def fromZio(name: ZioXmlName): XmlName =
    XmlName(name.localName, XmlNamespace.of(name.prefix, name.namespace))

package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.Xml as XML

// XML AST for ZIO Blocks XML
given Xml: XmlAst[XML.Element]:
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
    name = name.toZio(attributes, isAttribute = false),
    attributes = Chunk.from(attributes).map((attr, value) => (attr.toZio(attributes, isAttribute = true), value)),
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
    override def getName: XmlName = XmlName.fromZio(element.name)

    override def getChildren: Nodes =
      element.children

    override def getAttributes: Seq[(XmlName, String)] =
      element.attributes.map((name, value) => (XmlName.fromZio(name), value))

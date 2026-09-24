package org.podval.xml

import scala.annotation.unused
import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom as XML

// XML AST for ZIO Blocks HTML.
// `import ZioBlocksHtml.given` to convert (`element.to[ZioBlocksHtml.Element]`).
object ZioBlocksHtml extends XmlAst[XML.Element]:
  given ZioBlocksHtml.type = this

  override type Node = XML

  override def text(text: String): Node = XML.text(text)

  // HTML has no CDATA; the writer encodes `&` and `<` in the text.
  override def cdata(text: String): Node = ZioBlocksHtml.text(text)

  override def comment(text: String): Option[Node] = None

  override def processingInstruction(target: String, data: String): Option[Node] = None

  override def element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Nodes
  ): Element = XML.Element.Generic(
    tag = name.qName,
    children = Chunk.from(children),
    attributes = Chunk.from(attributes).map((name, value) => XML.Attribute.KeyValue(
      name.qName,
      XML.AttributeValue.StringValue(value)
    ))
  )

  override def foldNode[A](
    node: Node,
    element: Element => A,
    text: String => A,
    @unused cdata: String => A,
    @unused comment: String => A,
    @unused processingInstruction: (String, String) => A,
    unknown: => A
  ): A = node match
    case value: XML.Element => element(value)
    case XML.Text(content) => text(content)
    case _ => unknown

  override def nameOf(element: Element): XmlName = XmlName.parseQName(element.tag)

  override def childrenOf(element: Element): Nodes = element.children

  /** Merge ZIO Blocks multi-valued attrs (`className += …` is an AppendValue)
    * as Dom.render does: last `:=` is the base, then every `+=` in order.
    * One pair per name, sorted by name. Boolean attributes pass through.
    */
  override def attributesOf(element: Element): Seq[(XmlName, String)] =
    Chunk.from(element.attributes.groupBy(attributeName).view.mapValues(mergeAttribute))
      .sortBy(_._1)
      .map((name, value) => (XmlName.parse(name, isAttribute = true), value))

  private def mergeAttribute(attributes: Chunk[XML.Attribute]): String =
    val base: Option[String] = attributes.collect {
      case XML.Attribute.KeyValue(_, value) => attributeValue(value)
    }.lastOption
    val extras: Chunk[(String, String)] = attributes.collect {
      case XML.Attribute.AppendValue(_, value, sep) => (sep.render, attributeValue(value))
    }
    if base.isDefined || extras.nonEmpty then
      (base ++ extras.map(_._2)).mkString(extras.headOption.fold("")(_._1))
    else
      attributes.collect {
        case XML.Attribute.BooleanAttribute(_, enabled) => enabled
      }.last.toString

  private def attributeName(attribute: XML.Attribute): String = attribute match
    case XML.Attribute.BooleanAttribute(name, _) => name
    case XML.Attribute.KeyValue(name, _)         => name
    case XML.Attribute.AppendValue(name, _, _)   => name

  private def attributeValue(value: XML.AttributeValue): String = value match
    case XML.AttributeValue.StringValue(value) => value
    case XML.AttributeValue.BooleanValue(value) => value.toString
    case XML.AttributeValue.MultiValue(values, separator) => values.mkString(separator.render)
    case XML.AttributeValue.JsValue(value) => value.value

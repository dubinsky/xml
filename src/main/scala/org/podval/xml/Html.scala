package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom as XML

// XML AST for ZIO Blocks HTML. Not a package given: `import Html.given`.
object Html extends XmlAst[XML.Element]:
  given Html.type = this

  override type Node = XML

  override def text(text: String): Node = XML.text(text)

  // HTML has no CDATA; the writer encodes `&` and `<` in the text.
  override def cdata(text: String): Node = Html.text(text)

  override def element(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)],
    children: Nodes
  ): Element = XML.Element.Generic(
    tag = name.qName,
    attributes = Chunk.from(attributes).map((name, value) => mkAttribute(name.qName, value)),
    children = Chunk.from(children)
  )

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: XML.Element => Some(element)
      case _ => None

    override def asCData: Option[String] = None

    override def asText: Option[String] = node match
      case XML.Text(content) => Some(content)
      case _ => None

    override def asAtom: Option[String] = node.asText

    override def asComment: Option[String] = None

    override def asProcessingInstruction: Option[(String, String)] = None

  extension (element: Element)
    override def getExpandedName: XmlExpandedName = XmlExpandedName.parseQName(element.tag)

    /**
     * Merge ZIO Blocks multi-valued attrs (`className += …` is an AppendValue)
     * as Dom.render does: last `:=` is the base, then every `+=` in order.
     * One pair per name, sorted by name. Boolean attributes pass through.
     */
    override def getExpandedAttributes: Seq[(XmlExpandedName, String)] =
      Chunk.from(element.attributes.groupBy(attributeName).view.mapValues(mergeAttribute))
        .sortBy(_._1)
        .map((name, value) => (XmlExpandedName.parse(name, isAttribute = true), value))

    override def getChildren: Nodes = element.children

  private def mkAttribute(name: String, value: String) = XML.Attribute.KeyValue(
    name,
    XML.AttributeValue.StringValue(value)
  )

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

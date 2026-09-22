package org.podval.xml

/** Node of the XML tree owned by this library. */
sealed trait XmlNode derives CanEqual

/** Comment or processing instruction outside the document element (`Misc` minus whitespace). */
sealed trait XmlMisc extends XmlNode derives CanEqual:
  def markup: String

object XmlNode:
  final case class Element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Seq[XmlNode]
  ) extends XmlNode derives CanEqual

  final case class Text(value: String) extends XmlNode derives CanEqual

  final case class CData(value: String) extends XmlNode derives CanEqual

  final case class Comment(value: String) extends XmlMisc derives CanEqual:
    def markup: String = s"<!--$value-->"

  final case class ProcessingInstruction(target: String, data: String) extends XmlMisc derives CanEqual:
    def markup: String =
      if data.isEmpty then s"<?$target?>" else s"<?$target $data?>"

// XML AST owned by this library.
// `import Xml.given` to parse/write this AST.
// Convert with `element.to[Xml.Element]`.
object Xml extends XmlAst[XmlNode.Element], XmlAstDsl[XmlNode.Element]:
  given Xml.type = this

  override type Node = XmlNode

  override def text(text: String): Node = XmlNode.Text(text)

  override def cdata(text: String): Node = XmlNode.CData(text)

  override def comment(text: String): Option[Node] = Some(XmlNode.Comment(text))

  override def processingInstruction(target: String, data: String): Option[Node] =
    Some(XmlNode.ProcessingInstruction(target, data))

  override def element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Nodes
  ): Element = XmlNode.Element(name, attributes, children)

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: XmlNode.Element => Some(element)
      case _ => None

    override def asText: Option[String] = node match
      case XmlNode.Text(value) => Some(value)
      case _ => None

    override def asCData: Option[String] = node match
      case XmlNode.CData(value) => Some(value)
      case _ => None

    override def asComment: Option[String] = node match
      case XmlNode.Comment(value) => Some(value)
      case _ => None

    override def asProcessingInstruction: Option[(String, String)] = node match
      case XmlNode.ProcessingInstruction(target, data) => Some((target, data))
      case _ => None

    override def asAtom: Option[String] = node.asText.orElse(node.asCData)

  extension (element: Element)
    override def getName: XmlName = element.name

    override def getChildren: Nodes = element.children

    override def getAttributes: Seq[(XmlName, String)] = element.attributes

package org.podval.xml

import scala.annotation.unused

/** Node of the XML tree owned by this library. */
sealed trait XmlNode derives CanEqual:
  def asElement: Option[XmlNode.Element] = this match
    case element: XmlNode.Element => Some(element)
    case _ => None

  def asText: Option[String] = this match
    case XmlNode.Text(value) => Some(value)
    case _ => None

  def asCData: Option[String] = this match
    case XmlNode.CData(value) => Some(value)
    case _ => None

  def asComment: Option[String] = this match
    case XmlNode.Comment(value) => Some(value)
    case _ => None

  def asProcessingInstruction: Option[(String, String)] = this match
    case XmlNode.ProcessingInstruction(target, data) => Some((target, data))
    case _ => None

  def asAtom: Option[String] = asText.orElse(asCData)

  def fold[A](
    element: XmlNode.Element => A,
    text: String => A,
    cdata: String => A,
    comment: String => A,
    processingInstruction: (String, String) => A,
    @unused unknown: => A
  ): A = XmlNode.fold(this, element, text, cdata, comment, processingInstruction)

  def isWhitespace: Boolean = asText.exists(_.trim.isEmpty)

  def isCharacters: Boolean = asCData.isDefined || asText.exists(_.trim.nonEmpty)

  def getText: String = asAtom
    .orElse(asElement.map(el => el.getChildren.map(_.getText).mkString))
    .getOrElse("")

/** Comment or processing instruction outside the document element (`Misc` minus whitespace). */
sealed trait XmlMisc extends XmlNode derives CanEqual:
  def markup: String

object XmlNode:
  def fold[A](
    node: XmlNode,
    element: Element => A,
    text: String => A,
    cdata: String => A,
    comment: String => A,
    processingInstruction: (String, String) => A
  ): A = node match
    case elementNode: Element => element(elementNode)
    case Text(value) => text(value)
    case CData(value) => cdata(value)
    case Comment(value) => comment(value)
    case ProcessingInstruction(target, data) => processingInstruction(target, data)

  final case class Element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Seq[XmlNode]
  ) extends XmlNode, XmlElementApi derives CanEqual:
    def getName: XmlName = name

    def getChildren: Seq[XmlNode] = children

    def getAttributes: Seq[(XmlName, String)] = attributes

    def setChildren(children: Seq[XmlNode]): Element = copy(children = children)

    def setText(text: String): Element = setChildren(Seq(Text(text)))

    def isElement(elem: XmlElement): Boolean = name.is(elem)

    def isNamed(name: String): Boolean = this.name.matches(name)

  final case class Text(value: String) extends XmlNode derives CanEqual

  final case class CData(value: String) extends XmlNode derives CanEqual

  final case class Comment(value: String) extends XmlMisc derives CanEqual:
    def markup: String = s"<!--$value-->"

  final case class ProcessingInstruction(target: String, data: String) extends XmlMisc derives CanEqual:
    def markup: String =
      if data.isEmpty then s"<?$target?>" else s"<?$target $data?>"

  /** `flatMap` that builds a `Seq`, so a text node followed by an element does not depend on `Chunk`'s `ClassTag`.
    * Import `XmlNode.flatMapNodes` or `XmlNode.convertElements` (also exported from `dsl`). No `XmlAst` given.
    */
  extension (nodes: Seq[XmlNode])
    def flatMapNodes(f: XmlNode => Seq[XmlNode]): Seq[XmlNode] =
      val buf = List.newBuilder[XmlNode]
      nodes.foreach(node => buf.addAll(f(node)))
      buf.result()

    def convertElements(converter: Element => Option[Seq[XmlNode]]): Seq[XmlNode] =
      val buf = List.newBuilder[XmlNode]
      nodes.foreach: node =>
        buf.addAll(node.asElement.flatMap(converter).getOrElse(Seq(node)))
      buf.result()

// XML AST owned by this library.
// `XmlParser` and `XmlWriterConfig.render` take `Xml.Element` and need no given.
// `import Xml.given` to convert another tree with `to[Xml.Element]`.
// Walk, attribute, and CSS operations are members of `XmlNode.Element`.
// `import XmlNode.flatMapNodes` (or `convertElements`) for a node list; no given.
object Xml extends XmlAst[XmlNode.Element], XmlAstDsl[XmlNode.Element]:
  /** What [[XmlNode.Element.rewrite]] does with the element it visits.
    * `Keep` and `Replace` are walked again.
    * `Emit` is inserted as finished output.
    */
  enum Rewrite:
    case Keep(element: XmlNode.Element)
    case Replace(nodes: Seq[XmlNode])
    case Emit(nodes: Seq[XmlNode])

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

  override def foldNode[A](
    node: Node,
    element: Element => A,
    text: String => A,
    cdata: String => A,
    comment: String => A,
    processingInstruction: (String, String) => A,
    @unused unknown: => A
  ): A = XmlNode.fold(node, element, text, cdata, comment, processingInstruction)

  override def nameOf(element: Element): XmlName = element.name

  override def childrenOf(element: Element): Nodes = element.children

  override def attributesOf(element: Element): Seq[(XmlName, String)] = element.attributes

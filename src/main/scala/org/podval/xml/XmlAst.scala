package org.podval.xml

/** AST that represents XML.
  * Abstracts over the underlying representation:
  * - [[Xml]], the tree owned by this library
  * - ZIO Blocks XML
  * - ZIO Blocks HTML
  * - Scala XML
  *
  * Construction (`div`, `:=`, `.when`), walks, attributes, and CSS live on [[Xml]].
  * The parser, writer, and builder build [[Xml]] only.
  * This trait is rebuild and `to[TO]`.
  * Each tree implements [[foldNode]], [[nameOf]], [[childrenOf]], and [[attributesOf]].
  * The extensions below are derived from those four.
  */
trait XmlAst[ELEMENT]:
  final type Element = ELEMENT

  type Node >: Element

  final type Nodes = Seq[Node]

  def text(text: String): Node

  def cdata(text: String): Node

  /** `None` if this AST cannot represent comments (HTML). */
  def comment(text: String): Option[Node]

  /** `None` if this AST cannot represent processing instructions (HTML). */
  def processingInstruction(target: String, data: String): Option[Node]

  final def element(elem: XmlElement): Element = element(elem.name, Seq.empty, Seq.empty)

  final def element(name: String): Element = element(name, Seq.empty, Seq.empty)

  final def element(name: String, attributes: Seq[(String, String)], children: Nodes): Element = element(
    XmlName.parse(name, attributes, isAttribute = false),
    XmlName.attributes(attributes),
    children
  )

  def element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Nodes
  ): Element

  /** One match over this tree's node kinds. */
  def foldNode[A](
    node: Node,
    element: Element => A,
    text: String => A,
    cdata: String => A,
    comment: String => A,
    processingInstruction: (String, String) => A,
    unknown: => A
  ): A

  def nameOf(element: Element): XmlName

  def childrenOf(element: Element): Nodes

  def attributesOf(element: Element): Seq[(XmlName, String)]

  final def withName(element: Element, name: String): Element = this.element(
    name = XmlName.parseDeclared(
      name,
      element.getAttributes,
      isAttribute = false,
      existing = Some(element.getName)
    ),
    attributes = element.getAttributes,
    children = element.getChildren
  )

  final def withChildren(element: Element, children: Nodes): Element = this.element(
    name = element.getName,
    attributes = element.getAttributes,
    children = children
  )

  final def withAttributes(
    element: Element,
    attributes: Seq[(XmlName, String)]
  ): Element = this.element(
    name = XmlName.parseDeclared(element.getName, attributes, isAttribute = false),
    attributes = attributes,
    children = element.getChildren
  )

  final def withAttribute(element: Element, attribute: String, value: String): Element = withAttribute(
    element,
    XmlName.parseDeclared(attribute, element.getAttributes, isAttribute = true),
    value
  )

  final def withAttribute(element: Element, attribute: XmlName, value: String): Element = withAttributes(
    element,
    attributes =
      val other: Seq[(XmlName, String)] =
        element.getAttributes.filterNot((name, _) => name.sameAs(attribute))
      if value.nonEmpty then other.appended(attribute -> value) else other
  )

  // Concatenate only: text nodes already carry author whitespace. Joining with a space
  // puts a gap before punctuation after inline markup (`</persName>,` → "е ,").
  final def toString(nodes: Nodes): String = nodes.map(_.getText).mkString

  /** Rebuild `element` in another `XmlAst`. Nodes the destination cannot represent are dropped.
    * Prefer `element.to[TO]` except on Scala XML, whose `NodeSeq.to` shadows the extension. */
  final def converted[TO](element: Element)(using dest: XmlAst[TO]): TO = dest.element(
    element.getName,
    element.getAttributes,
    toNodes(element.getChildren)
  )

  private def toNodes[TO](children: Nodes)(using dest: XmlAst[TO]): dest.Nodes =
    val buf = List.newBuilder[dest.Node]
    children.foreach: child =>
      child.fold(
        element = el => Some(converted(el)),
        text = t => Some(dest.text(t)),
        cdata = t => Some(dest.cdata(t)),
        comment = t => dest.comment(t),
        processingInstruction = (target, data) => dest.processingInstruction(target, data),
        unknown = None
      ).foreach(node => buf += node)
    buf.result()

  extension (node: Node)
    def asElement: Option[Element] =
      foldNode(node, Some(_), _ => None, _ => None, _ => None, (_, _) => None, None)

    def asText: Option[String] =
      foldNode(node, _ => None, Some(_), _ => None, _ => None, (_, _) => None, None)

    def asCData: Option[String] =
      foldNode(node, _ => None, _ => None, Some(_), _ => None, (_, _) => None, None)

    def asComment: Option[String] =
      foldNode(node, _ => None, _ => None, _ => None, Some(_), (_, _) => None, None)

    def asProcessingInstruction: Option[(String, String)] =
      foldNode(node, _ => None, _ => None, _ => None, _ => None, (target, data) => Some((target, data)), None)

    def asAtom: Option[String] = node.asText.orElse(node.asCData)

    def fold[A](
      element: Element => A,
      text: String => A,
      cdata: String => A,
      comment: String => A,
      processingInstruction: (String, String) => A,
      unknown: => A
    ): A = foldNode(node, element, text, cdata, comment, processingInstruction, unknown)

    def isWhitespace: Boolean = node.asText.exists(_.trim.isEmpty)

    def isCharacters: Boolean = node.asCData.isDefined || node.asText.exists(_.trim.nonEmpty)

    def getText: String = node.asAtom
      .orElse(node.asElement.map(_.getChildren).map(toString))
      .getOrElse("")

  extension (element: Element)
    def getName: XmlName = nameOf(element)

    def to[TO: XmlAst]: TO = converted(element)

    def getChildren: Nodes = childrenOf(element)

    def getAttributes: Seq[(XmlName, String)] = attributesOf(element)

object XmlAst:
  def toId(text: String): String = text.trim.replace(' ', '-')

  def parseBoolean(raw: String): Boolean = raw.trim.toLowerCase match
    case "true" | "yes" | "1" => true
    case "false" | "no" | "0" => false
    case other => throw XmlError(s"Invalid boolean: $other")

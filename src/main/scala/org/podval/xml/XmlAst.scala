package org.podval.xml

object XmlAst:
  def toId(text: String): String = text.trim.replace(' ', '-')

  def parseBoolean(raw: String): Boolean = raw.trim.toLowerCase match
    case "true" | "yes" | "1" => true
    case "false" | "no" | "0" => false
    case other => throw XmlError(s"Invalid boolean: $other")

/** AST that represents XML and provides operations on it;
  * abstracts over the underlying representation:
  * - ZIO Blocks XML
  * - ZIO Blocks HTML
  * - Scala XML
  */
trait XmlAst[ELEMENT] extends XmlAstWalk[ELEMENT], XmlAstCssClass[ELEMENT]:
  final type Element = ELEMENT

  type Node >: Element

  final type Nodes = Seq[Node]

  def text(text: String): Node

  def cdata(text: String): Node

  /** `None` if this AST cannot represent comments (HTML). */
  def comment(text: String): Option[Node] = None

  /** `None` if this AST cannot represent processing instructions (HTML). */
  def processingInstruction(target: String, data: String): Option[Node] = None

  final def element(elem: XmlElement): Element = element(elem.expanded, Seq.empty, Seq.empty)

  final def element(name: String): Element = element(name, Seq.empty, Seq.empty)

  final def element(name: String, attributes: Seq[(String, String)], children: Nodes): Element = element(
    XmlExpandedName.parse(name, attributes, isAttribute = false),
    XmlExpandedName.attributes(attributes),
    children
  )

  def element(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)],
    children: Nodes
  ): Element

  final def renamed(element: Element, name: String): Element = this.element(
    XmlExpandedName.parseDeclared(
      name,
      element.getAttributes,
      isAttribute = false,
      existing = Some(element.getName)
    ),
    element.getAttributes,
    element.getChildren
  )

  final def withChildren(element: Element, children: Nodes): Element =
    this.element(element.getName, element.getAttributes, children)

  final def withAttributes(
    element: Element,
    attributes: Seq[(XmlExpandedName, String)]
  ): Element = this.element(
    XmlExpandedName.parseDeclared(
      element.qName,
      attributes,
      isAttribute = false,
      existing = Some(element.getName)
    ),
    attributes,
    element.getChildren
  )

  final def withAttribute(element: Element, attribute: String, value: String): Element =
    val parsed: XmlExpandedName = XmlExpandedName.parseDeclared(
      attribute,
      element.getAttributes,
      isAttribute = true
    )
    val other: Seq[(XmlExpandedName, String)] =
      element.getAttributes.filterNot((name, _) => name.sameAs(parsed))
    val attrs: Seq[(XmlExpandedName, String)] =
      if value.nonEmpty then other.appended(parsed -> value) else other
    withAttributes(element, attrs)

  // Concatenate only: text nodes already carry author whitespace. Joining with a space
  // puts a gap before punctuation after inline markup (`</persName>,` → "е ,").
  final def toString(nodes: Nodes): String = nodes.map(_.getText).mkString

  /** Rebuild `element` in another `XmlAst`. Nodes the destination cannot represent are dropped.
    * Prefer `element.to[TO]` except on Scala XML, whose `NodeSeq.to` shadows the extension. */
  final def converted[TO](element: Element)(using dest: XmlAst[TO]): TO =
    dest.element(
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
    def fold[A](
      element: Element => A,
      text: String => A,
      cdata: String => A,
      comment: String => A,
      processingInstruction: (String, String) => A,
      unknown: => A
    ): A =
      node.asElement.map(element)
        .orElse(node.asCData.map(cdata))
        .orElse(node.asText.map(text))
        .orElse(node.asComment.map(comment))
        .orElse(node.asProcessingInstruction.map(processingInstruction.tupled))
        .getOrElse(unknown)

    def asElement: Option[Element]

    def asAtom: Option[String]

    def asText: Option[String]

    def asCData: Option[String]

    def asComment: Option[String]

    def asProcessingInstruction: Option[(String, String)]

    def isWhitespace: Boolean = node.asText.exists(_.trim.isEmpty)

    def isCharacters: Boolean = node.asCData.isDefined || node.asText.exists(_.trim.nonEmpty)

    def getText: String = node
      .asAtom
      .orElse(node.asElement.map(_.getChildren).map(toString))
      .getOrElse("")

  extension (element: Element)
    def getName: XmlExpandedName

    def qName: String = element.getName.qName

    def localName: String = element.getName.localName

    def getPrefix: Option[String] = element.getName.prefix

    def getNamespace: Option[String] = element.getName.uri

    // TODO name this and above consistently or remove
    def rename(name: String): Element = renamed(element, name)

    def renameKeepingClass(name: String): Element =
      element.addClass(element.localName).rename(name)

    def isElement(elem: XmlElement): Boolean =
      element.localName == elem.expanded.localName &&
        element.getPrefix == elem.expanded.prefix

    def isA: Boolean = isElement(XmlElement.A)

    def to[TO: XmlAst]: TO = converted(element)

    def getChildren: Nodes

    def setChildren(children: Nodes): Element = withChildren(element, children)

    def setText(text: String): Element = element.setChildren(Seq(this.text(text)))

    def getTextOpt: Option[String] = Option.when(element.getChildren.nonEmpty)(element.getText)

    def flatMapElements[A](f: Element => Seq[A]): Seq[A] = element
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element => f(element))

    def getAttributes: Seq[(XmlExpandedName, String)]

    def setAttributes(attributes: Seq[(XmlExpandedName, String)]): Element =
      withAttributes(element, attributes)

    def get(attribute: XmlAttribute): Option[String] =
      element.getAttributes.collectFirst:
        case (n, v) if n.sameAs(attribute.expanded) => v

    def get(attribute: String): Option[String] =
      element.getAttributes.collectFirst:
        case (n, v) if n.qName == attribute => v

    def set(attribute: XmlAttribute, value: String): Element =
      set(attribute.qName, value)

    def set(attribute: String, value: String): Element =
      withAttribute(element, attribute, value)

    def set(attribute: XmlAttribute, value: Option[String]): Element =
      set(attribute.qName, value)

    def set(attribute: String, value: Option[String]): Element =
      value.fold(element)(element.set(attribute, _))

    def getId: Option[String] = get(XmlAttribute.Id)

    def setId(value: String): Element = set(XmlAttribute.Id, value)

    def setId(value: Option[String]): Element = set(XmlAttribute.Id, value)

    def copyXmlId: Element =
      if element.getId.exists(_.nonEmpty)
      then element
      else element.setId(element.get(XmlAttribute.XmlId).filter(_.nonEmpty))

    def getHref: Option[String] = get(XmlAttribute.Href)

    def setHref(value: String): Element = set(XmlAttribute.Href, value)

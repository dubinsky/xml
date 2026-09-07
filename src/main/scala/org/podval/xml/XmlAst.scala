package org.podval.xml

// AST that represents XML and provides operations on it;
// abstracts over the underlying representation:
// - ZIO Blocks XML
// - ZIO Blocks HTML
// - Scala XML
// - potentially DOM
trait XmlAst[ELEMENT]:
  final type Element = ELEMENT

  type Node >: Element

  final type Nodes = Seq[Node]

  def text(text: String): Node

  def cdata(text: String): Node

  /** `None` if this AST cannot represent comments (HTML). */
  def comment(text: String): Option[Node] = None

  /** `None` if this AST cannot represent processing instructions (HTML). */
  def processingInstruction(target: String, data: String): Option[Node] = None

  final def element(elem: XmlElement): Element = element(elem.name)

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
    XmlExpandedName.parse(
      name,
      element.getAttributes,
      isAttribute = false,
      existing = Some(element.getExpandedName)
    ),
    element.getExpandedAttributes,
    element.getChildren
  )

  final def withChildren(element: Element, children: Nodes): Element =
    this.element(element.getExpandedName, element.getExpandedAttributes, children)

  final def withAttributes(element: Element, attributes: Seq[(String, String)]): Element = this.element(
    XmlExpandedName.parse(
      element.getName,
      attributes,
      isAttribute = false,
      existing = Some(element.getExpandedName)
    ),
    XmlExpandedName.attributes(attributes),
    element.getChildren
  )

  final def withAttribute(element: Element, attribute: String, value: String): Element =
    val parsed: XmlExpandedName = XmlExpandedName.parse(
      attribute,
      element.getAttributes,
      isAttribute = true
    )
    val other: Seq[(XmlExpandedName, String)] =
      element.getExpandedAttributes.filterNot((name, _) => name.qualifiedName == parsed.qualifiedName)
    val attrs: Seq[(XmlExpandedName, String)] =
      if value.nonEmpty then other.appended(parsed -> value) else other
    this.element(
      XmlExpandedName.parse(
        element.getName,
        XmlExpandedName.asPairs(attrs),
        isAttribute = false,
        existing = Some(element.getExpandedName)
      ),
      attrs,
      element.getChildren
    )

  // Concatenate only: text nodes already carry author whitespace. Joining with a space
  // puts a gap before punctuation after inline markup (`</persName>,` → "е ,").
  final def toString(nodes: Nodes): String = nodes.map(_.getText).mkString

  /** Rebuild `element` in another `XmlAst`. Nodes the destination cannot represent are dropped.
    * Prefer `element.to[TO]` except on Scala XML, whose `NodeSeq.to` shadows the extension. */
  final def converted[TO](element: Element)(using dest: XmlAst[TO]): TO =
    dest.element(
      element.getExpandedName,
      element.getExpandedAttributes,
      toNodes(element.getChildren)
    )

  private def toNodes[TO](children: Nodes)(using dest: XmlAst[TO]): dest.Nodes =
    val buf = List.newBuilder[dest.Node]
    children.foreach: child =>
      child.asElement.map(converted(_))
        .orElse(child.asCData.map(dest.cdata))
        .orElse(child.asText.map(dest.text))
        .orElse(child.asComment.flatMap(dest.comment))
        .orElse(child.asProcessingInstruction.flatMap((target, data) => dest.processingInstruction(target, data)))
        .foreach(node => buf += node)
    buf.result()

  // Conversions
  extension (node: Node)
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

  // Element name
  extension (element: Element)
    def getExpandedName: XmlExpandedName

    def getName: String = element.getExpandedName.qualifiedName

    def localName: String = element.getExpandedName.localName

    def getPrefix: Option[String] = element.getExpandedName.prefix

    def getNamespace: Option[String] = element.getExpandedName.namespace

    def rename(name: String): Element = renamed(element, name)

    def isElement(elem: XmlElement): Boolean = element.getName == elem.name

    def isA: Boolean = isElement(XmlElement.A)

    def to[TO: XmlAst]: TO = converted(element)

  // Children
  extension (element: Element)
    def getChildren: Nodes

    def setChildren(children: Nodes): Element = withChildren(element, children)

    def setText(text: String): Element = element.setChildren(Seq(this.text(text)))

    // Remove markup
    def getTextOpt: Option[String] = Option.when(element.getChildren.nonEmpty)(element.getText)

    def flatMapElements[A](f: Element => Seq[A]): Seq[A] = element
      .getChildren
      .flatMap(_.asElement)
      .flatMap(element => f(element))

    def transform(
      transformElement: Element => Element,
      stopAtCode: Boolean = true
    ): Element =
      def loop(element: Element): Element =
        if stopAtCode && element.getName == "code" then element
        else
          val result: Element = transformElement(element)
          result.setChildren(result.getChildren.map(xml => xml.asElement.fold(xml)(loop)))
      loop(element)

    def gather[A](
      gatherElement: Element => Option[A],
      stopAtCode: Boolean = true
    ): Seq[A] =
      def loop(element: Element): Seq[A] =
        val fromElement: Option[A] = gatherElement(element)
        val fromChildren: Seq[A] =
          if stopAtCode && element.getName == "code" then Seq.empty
          else element.flatMapElements(loop)
        fromElement.toSeq ++ fromChildren
      loop(element)

    def gatherWithContext[A](
      gatherElement: (Element, Option[Element]) => Option[A],
      isContext: Element => Boolean,
      stopAtCode: Boolean = true
    ): Seq[A] =
      def loop(element: Element, context: Option[Element]): Seq[A] =
        val fromElement: Option[A] = gatherElement(element, context)
        val fromChildren: Seq[A] =
          if stopAtCode && element.getName == "code" then Seq.empty
          else
            val contextNew: Option[Element] = if isContext(element) then Some(element) else context
            element.flatMapElements(loop(_, contextNew))
        fromElement.toSeq ++ fromChildren
      loop(element, None)

    def gatherWithParent[A](
      gatherElement: (Element, Option[Element]) => Option[A],
      stopAtCode: Boolean = true
    ): Seq[A] =
      element.gatherWithContext(gatherElement, _ => true, stopAtCode)

  // Attributes
  extension (element: Element)
    def getExpandedAttributes: Seq[(XmlExpandedName, String)]

    def getAttributes: Seq[(String, String)] =
      XmlExpandedName.asPairs(element.getExpandedAttributes)

    def setAttributes(attributes: Seq[(String, String)]): Element =
      withAttributes(element, attributes)

    def get(attribute: XmlAttribute): Option[String] =
      get(attribute.name)

    def get(attribute: String): Option[String] =
      element.getAttributes.find(_._1 == attribute).map(_._2)

    def set(attribute: XmlAttribute, value: String): Element =
      set(attribute.name, value)

    def set(attribute: String, value: String): Element =
      withAttribute(element, attribute, value)

    def set(attribute: XmlAttribute, value: Option[String]): Element =
      set(attribute.name, value)

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

  // HTML 'class' attribute
  extension (element: Element)
    def getClasses: Seq[String] = element
      .get(HtmlClass)
      .fold(Seq.empty): element =>
        element
          .split(' ')
          .toIndexedSeq
          .map(_.trim)
          .filterNot(_.isEmpty)

    def setClasses(values: Seq[String]): Element =
      element.set(HtmlClass, values.mkString(" "))

    def has(htmlClass: HtmlClass): Boolean = hasClass(htmlClass.name)

    def hasClass(htmlClass: String): Boolean = element.getClasses.contains(htmlClass)

    def add(htmlClass: Option[HtmlClass]): Element =
      htmlClass.fold(element)(element.add)

    def add(htmlClass: HtmlClass): Element =
      addClass(htmlClass.name)

    def addClass(htmlClass: String): Element =
      val list = element.getClasses
      if list.contains(htmlClass)
      then element
      else element.setClasses(list.appended(htmlClass))

    def getPrefixedClasses(prefix: String): Seq[String] = element
      .getClasses
      .filter(_.startsWith(s"$prefix-"))
      .map(_.substring(prefix.length + 1))

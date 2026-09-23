package org.podval.xml

/** Element operations. The algorithms live here, on the owned tree.
  *
  * Parser, writer, and `to[TO]` stay on [[XmlAst]]. A foreign tree converts first.
  */
private[xml] trait XmlElementApi:
  this: XmlNode.Element =>

  def childElements: Seq[XmlNode.Element] = getChildren.flatMap(_.asElement)

  def rename(name: String): XmlNode.Element = Xml.withName(this, name)

  def renameKeepingClass(name: String): XmlNode.Element =
    addClass(getName.localName).rename(name)

  def isA: Boolean = isElement(XmlElement.A)

  def to[TO](using XmlAst[TO]): TO = Xml.converted(this)

  def getTextOpt: Option[String] = Option.when(getChildren.nonEmpty)(getText)

  def flatMapElements[A](f: XmlNode.Element => Seq[A]): Seq[A] = childElements.flatMap(f)

  def setAttributes(attributes: Seq[(XmlName, String)]): XmlNode.Element =
    Xml.withAttributes(this, attributes)

  def get(attribute: XmlAttribute): Option[String] =
    getAttributes.collectFirst:
      case (n, v) if n.is(attribute) => v

  def get(attribute: String): Option[String] =
    getAttributes.collectFirst:
      case (n, v) if n.qName == attribute => v

  def set(attribute: XmlAttribute, value: String): XmlNode.Element =
    Xml.withAttribute(this, attribute.name, value)

  def set(attribute: String, value: String): XmlNode.Element =
    Xml.withAttribute(this, attribute, value)

  def set(attribute: XmlAttribute, value: Option[String]): XmlNode.Element =
    value.fold(this)(set(attribute, _))

  def set(attribute: String, value: Option[String]): XmlNode.Element =
    value.fold(this)(set(attribute, _))

  def getId: Option[String] = get(XmlAttribute.Id)

  def setId(value: String): XmlNode.Element = set(XmlAttribute.Id, value)

  def setId(value: Option[String]): XmlNode.Element = set(XmlAttribute.Id, value)

  def copyXmlId: XmlNode.Element =
    if getId.exists(_.nonEmpty) then this
    else setId(get(XmlAttribute.XmlId).filter(_.nonEmpty))

  def getHref: Option[String] = get(XmlAttribute.Href)

  def setHref(value: String): XmlNode.Element = set(XmlAttribute.Href, value)

  def transform(
    transformElement: XmlNode.Element => XmlNode.Element,
    stopAtCode: Boolean = true
  ): XmlNode.Element =
    def loop(element: XmlNode.Element): XmlNode.Element =
      if stopAtCode && element.isNamed(XmlElement.Code.localName) then element
      else
        val result: XmlNode.Element = transformElement(element)
        result.setChildren(result.getChildren.map(xml => xml.asElement.fold(xml)(loop)))
    loop(this)

  def rewrite(
    f: (XmlNode.Element, Option[XmlNode.Element]) => Xml.Rewrite,
    stopAtCode: Boolean = true
  ): XmlNode.Element =
    def loop(element: XmlNode.Element, parent: Option[XmlNode.Element]): Seq[XmlNode] =
      if stopAtCode && element.isNamed(XmlElement.Code.localName) then Seq(element)
      else f(element, parent) match
        case Xml.Rewrite.Keep(result) =>
          Seq(result.setChildren(descend(result.getChildren, Some(result))))
        case Xml.Rewrite.Replace(nodes) =>
          descend(nodes, parent)
        case Xml.Rewrite.Emit(nodes) =>
          nodes
    def descend(nodes: Seq[XmlNode], parent: Option[XmlNode.Element]): Seq[XmlNode] =
      XmlNode.flatMapNodes(nodes): node =>
        node.asElement.fold(Seq(node))(el => loop(el, parent))
    loop(this, None) match
      case Seq(only) if only.asElement.nonEmpty => only.asElement.get
      case _ => throw XmlError("rewrite must leave exactly one element")

  /** Elements for which `predicate` is true. `stopAtCode` matches [[gather]]. */
  def elements(
    predicate: XmlNode.Element => Boolean,
    stopAtCode: Boolean = true
  ): Seq[XmlNode.Element] =
    gather(el => Option.when(predicate(el))(el), stopAtCode)

  def gather[A](
    gatherElement: XmlNode.Element => Option[A],
    stopAtCode: Boolean = true
  ): Seq[A] =
    def loop(element: XmlNode.Element): Seq[A] =
      val fromElement: Option[A] = gatherElement(element)
      val fromChildren: Seq[A] =
        if stopAtCode && element.isNamed(XmlElement.Code.localName) then Seq.empty
        else element.flatMapElements(loop)
      fromElement.toSeq ++ fromChildren
    loop(this)

  def gatherWithContext[A](
    gatherElement: (XmlNode.Element, Option[XmlNode.Element]) => Option[A],
    isContext: XmlNode.Element => Boolean,
    stopAtCode: Boolean = true
  ): Seq[A] =
    def loop(element: XmlNode.Element, context: Option[XmlNode.Element]): Seq[A] =
      val fromElement: Option[A] = gatherElement(element, context)
      val fromChildren: Seq[A] =
        if stopAtCode && element.isNamed(XmlElement.Code.localName) then Seq.empty
        else
          val contextNew: Option[XmlNode.Element] = if isContext(element) then Some(element) else context
          element.flatMapElements(loop(_, contextNew))
      fromElement.toSeq ++ fromChildren
    loop(this, None)

  def gatherWithParent[A](
    gatherElement: (XmlNode.Element, Option[XmlNode.Element]) => Option[A],
    stopAtCode: Boolean = true
  ): Seq[A] =
    gatherWithContext(gatherElement, _ => true, stopAtCode)

  def convertText(converter: String => Seq[XmlNode]): XmlNode.Element =
    setChildren(XmlNode.flatMapNodes(getChildren)(xml => xml.asText.fold(Seq(xml))(converter)))

  def copyAttribute(from: String, to: String): XmlNode.Element =
    get(from).fold(this)(set(to, _))

  def getById(id: String): Option[XmlNode.Element] =
    gather(el => Option.when(el.getId.contains(id))(el)).headOption

  def isInclude: Boolean =
    getName.isInclude && get(XmlAttribute.Href).exists(_.trim.nonEmpty)

  def childrenNamed(name: String): Seq[XmlNode.Element] = childElements.filter(_.isNamed(name))

  def childNamed(name: String): Option[XmlNode.Element] = childrenNamed(name).headOption

  def requireName(name: String): Unit =
    if !isNamed(name) then throw XmlError(s"Expected '$name', found '${getName.qName}'")

  def requireAttr(name: String): String =
    get(name).map(_.trim).filter(_.nonEmpty).getOrElse:
      throw XmlError(s"Missing attribute '$name'")

  def requireNoOther(allowed: Set[String]): Unit =
    val extra: Seq[String] = childElements.map(_.getName.localName).filterNot(allowed.contains)
    if extra.nonEmpty then throw XmlError(s"Unparsed elements: $extra")

  def positiveInt(name: String): Int =
    val raw: String = requireAttr(name)
    val n: Int = raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))
    if n <= 0 then throw XmlError(s"Non-positive integer: $n")
    n

  def getClasses: Seq[String] = get(XmlAttribute.CssClass)
    .fold(Seq.empty)(_
      .split(' ')
      .toIndexedSeq
      .map(_.trim)
      .filterNot(_.isEmpty)
    )

  def setClasses(values: Seq[String]): XmlNode.Element = set(XmlAttribute.CssClass, values.mkString(" "))

  def has(cssClass: CssClass): Boolean = hasClass(cssClass.name)

  def hasClass(cssClass: String): Boolean = getClasses.contains(cssClass)

  def add(cssClass: Option[CssClass]): XmlNode.Element = cssClass.fold(this)(add)

  def add(cssClass: CssClass): XmlNode.Element = addClass(cssClass.name)

  def addClass(cssClass: String): XmlNode.Element =
    val list: Seq[String] = getClasses
    if list.contains(cssClass) then this
    else setClasses(list.appended(cssClass))

  def getPrefixedClasses(prefix: String): Seq[String] = getClasses
    .filter(_.startsWith(s"$prefix-"))
    .map(_.drop(prefix.length + 1))

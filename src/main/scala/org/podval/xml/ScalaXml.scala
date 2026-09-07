package org.podval.xml

// XML AST for Scala XML
given ScalaXml: XmlAst[scala.xml.Elem]:
  override type Node = scala.xml.Node

  override def text(text: String): Node = scala.xml.Text(text)

  override def cdata(text: String): Node = scala.xml.PCData(text)

  override def comment(text: String): Option[Node] = Some(scala.xml.Comment(text))

  override def processingInstruction(target: String, data: String): Option[Node] =
    Some(scala.xml.ProcInstr(target, data))

  override def element(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)],
    children: Nodes
  ): Element =
    scala.xml.Elem(
      prefix = name.prefix.filter(_.nonEmpty).orNull,
      label = name.localName,
      attributes = toMetaData(attributes),
      scope = toScope(name, attributes),
      minimizeEmpty = false,
      child = children*
    )

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: scala.xml.Elem => Some(element)
      case _ => None

    override def asText: Option[String] = node match
      case text: scala.xml.Text => Some(text.data)
      case _ => None

    override def asCData: Option[String] = node match
      case cdata: scala.xml.PCData => Some(cdata.data)
      case _ => None

    override def asComment: Option[String] = node match
      case comment: scala.xml.Comment => Some(comment.commentText)
      case _ => None

    override def asProcessingInstruction: Option[(String, String)] = node match
      case pi: scala.xml.ProcInstr => Some((pi.target, pi.proctext))
      case _ => None

    override def asAtom: Option[String] = node.asText.orElse(node.asCData)

  extension (element: Element)
    override def getExpandedName: XmlExpandedName =
      fromScope(element.scope, element.prefix, element.label, isAttribute = false)

    override def getName: String = element.getExpandedName.qualifiedName

    override def localName: String = element.getExpandedName.localName

    override def getPrefix: Option[String] = element.getExpandedName.prefix

    override def getNamespace: Option[String] = element.getExpandedName.namespace

    override def rename(name: String): Element = renamed(element, name)

    override def getExpandedAttributes: Seq[(XmlExpandedName, String)] =
      element.attributes.iterator.map: attribute =>
        val prefix: String = attribute match
          case prefixed: scala.xml.PrefixedAttribute => prefixed.pre
          case _ => null
        (
          fromScope(element.scope, prefix, attribute.key, isAttribute = true),
          scala.xml.NodeSeq.fromSeq(attribute.value).text
        )
      .toSeq

    override def getAttributes: Seq[(String, String)] =
      XmlExpandedName.asPairs(element.getExpandedAttributes)

    override def setAttributes(attributes: Seq[(String, String)]): Element =
      withAttributes(element, attributes)

    override def set(attribute: String, value: String): Element =
      withAttribute(element, attribute, value)

    override def set(attribute: XmlAttribute, value: String): Element =
      withAttribute(element, attribute.name, value)

    override def getChildren: Nodes =
      element.child

    override def setChildren(children: Nodes): Element =
      withChildren(element, children)

  private def fromScope(
    scope: scala.xml.NamespaceBinding,
    prefix: String,
    local: String,
    isAttribute: Boolean
  ): XmlExpandedName =
    val p: Option[String] = Option(prefix).filter(_.nonEmpty)
    val namespace: Option[String] =
      XmlNamespace.wellKnown(p, local, isAttribute).orElse:
        if isAttribute && p.isEmpty then None
        else
          val key: String = p.orNull
          Option(scope.getURI(key)).filter(uri => uri != null && uri.nonEmpty)
    XmlExpandedName(local, p, namespace)

  // scala.xml rejects prefix ""; unprefixed names use null.
  private def toMetaData(attributes: Seq[(XmlExpandedName, String)]): scala.xml.MetaData =
    attributes.foldRight(scala.xml.Null: scala.xml.MetaData):
      case ((name, value), next) =>
        scala.xml.Attribute(name.prefix.filter(_.nonEmpty).orNull, name.localName, value, next)

  // `xmlns*` stay attributes so the writer still emits them. Bindings also come
  // from expanded names so a child without its own xmlns keeps the URI.
  private def toScope(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)]
  ): scala.xml.NamespaceBinding =
    val declared: scala.xml.NamespaceBinding =
      attributes.foldLeft(xmlScope):
        case (scope, (n, uri)) if n.qualifiedName == "xmlns" =>
          scala.xml.NamespaceBinding(null, uri, scope)
        case (scope, (n, uri)) if n.prefix.contains("xmlns") =>
          scala.xml.NamespaceBinding(n.localName, uri, scope)
        case (scope, _) => scope
    (name +: attributes.map(_._1)).foldLeft(declared)(bind)

  private def bind(
    scope: scala.xml.NamespaceBinding,
    name: XmlExpandedName
  ): scala.xml.NamespaceBinding =
    name.namespace match
      case None => scope
      case Some(_) if name.prefix.contains("xmlns") => scope
      case Some(_) if name.prefix.isEmpty && name.localName == "xmlns" => scope
      case Some(uri) =>
        val prefix: String = name.prefix.filter(_.nonEmpty).orNull
        if Option(scope.getURI(prefix)).contains(uri) then scope
        else scala.xml.NamespaceBinding(prefix, uri, scope)

  private val xmlScope: scala.xml.NamespaceBinding =
    scala.xml.NamespaceBinding("xml", XmlNamespace.xml, scala.xml.TopScope)

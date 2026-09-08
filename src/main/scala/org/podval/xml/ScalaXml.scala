package org.podval.xml

import scala.xml.{Attribute, Comment, Elem, MetaData, NamespaceBinding, NodeSeq, PCData, PrefixedAttribute, ProcInstr,
  Text, TopScope}

// XML AST for Scala XML. Not a package given: `import ScalaXml.given`.
object ScalaXml extends XmlAst[Elem]:
  given ScalaXml.type = this

  override type Node = scala.xml.Node

  override def text(text: String): Node = Text(text)

  override def cdata(text: String): Node = PCData(text)

  override def comment(text: String): Option[Node] = Some(Comment(text))

  override def processingInstruction(target: String, data: String): Option[Node] = Some(ProcInstr(target, data))

  override def element(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)],
    children: Nodes
  ): Element = Elem(
    prefix = name.prefix.filter(_.nonEmpty).orNull,
    label = name.localName,
    attributes = toMetaData(attributes),
    scope = toScope(name, attributes),
    minimizeEmpty = false,
    child = children*
  )

  extension (node: Node)
    override def asElement: Option[Element] = node match
      case element: Elem => Some(element)
      case _ => None

    override def asText: Option[String] = node match
      case text: Text => Some(text.data)
      case _ => None

    override def asCData: Option[String] = node match
      case cdata: PCData => Some(cdata.data)
      case _ => None

    override def asComment: Option[String] = node match
      case comment: Comment => Some(comment.commentText)
      case _ => None

    override def asProcessingInstruction: Option[(String, String)] = node match
      case pi: ProcInstr => Some((pi.target, pi.proctext))
      case _ => None

    override def asAtom: Option[String] = node.asText.orElse(node.asCData)

  extension (element: Element)
    override def getName: XmlExpandedName = fromScope(
      element.scope, 
      element.prefix, 
      element.label, 
      isAttribute = false
    )

    override def getAttributes: Seq[(XmlExpandedName, String)] =
      element.attributes.iterator.map: attribute =>
        (
          fromScope(
            element.scope,
            attribute match
              case prefixed: PrefixedAttribute => prefixed.pre
              case _ => null,
            attribute.key,
            isAttribute = true
          ),
          NodeSeq.fromSeq(attribute.value).text
        )
      .toSeq

    override def getChildren: Nodes =
      element.child

  private def fromScope(
    scope: NamespaceBinding,
    prefix: String,
    local: String,
    isAttribute: Boolean
  ): XmlExpandedName =
    val p: Option[String] = Option(prefix).filter(_.nonEmpty)
    val namespace: Option[String] =
      XmlNamespace.wellKnown(p, local, isAttribute).map(_.uri).orElse:
        if isAttribute && p.isEmpty then None
        else
          val key: String = p.orNull
          Option(scope.getURI(key)).filter(uri => uri != null && uri.nonEmpty)
    XmlExpandedName(local, XmlNamespace.of(p, namespace))

  // scala.xml rejects prefix ""; unprefixed names use null.
  private def toMetaData(attributes: Seq[(XmlExpandedName, String)]): MetaData =
    attributes.foldRight(scala.xml.Null: MetaData):
      case ((name, value), next) =>
        Attribute(name.prefix.filter(_.nonEmpty).orNull, name.localName, value, next)

  // `xmlns*` stay attributes so the writer still emits them. Bindings also come
  // from expanded names so a child without its own xmlns keeps the URI.
  private def toScope(
    name: XmlExpandedName,
    attributes: Seq[(XmlExpandedName, String)]
  ): NamespaceBinding =
    val declared: NamespaceBinding =
      attributes.foldLeft(xmlScope):
        case (scope, (n, uri)) if n.isDefaultXmlns =>
          NamespaceBinding(null, uri, scope)
        case (scope, (n, uri)) if n.isXmlnsDeclaration =>
          NamespaceBinding(n.localName, uri, scope)
        case (scope, _) => scope
    (name +: attributes.map(_._1)).foldLeft(declared)(bind)

  private def bind(
    scope: NamespaceBinding,
    name: XmlExpandedName
  ): NamespaceBinding = name.uri match
    case None => scope
    case Some(_) if name.isXmlnsDeclaration => scope
    case Some(uri) =>
      val prefix: String = name.prefix.filter(_.nonEmpty).orNull
      if Option(scope.getURI(prefix)).contains(uri) then scope
      else NamespaceBinding(prefix, uri, scope)

  private val xmlScope: NamespaceBinding = NamespaceBinding(
    XmlNamespace.xml.prefix.orNull,
    XmlNamespace.xml.uri,
    TopScope
  )

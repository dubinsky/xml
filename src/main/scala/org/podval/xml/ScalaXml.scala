package org.podval.xml

import scala.xml.{Attribute, Comment, Elem, MetaData, NamespaceBinding, NodeSeq, PCData, PrefixedAttribute, ProcInstr,
  Text, TopScope}

// XML AST for Scala XML.
// `import ScalaXml.given` to convert (`element.to[ScalaXml.Element]`).
// `NodeSeq.to` shadows the extension; use `ScalaXml.converted`.
object ScalaXml extends XmlAst[Elem]:
  given ScalaXml.type = this

  override type Node = scala.xml.Node

  override def text(text: String): Node = Text(text)

  override def cdata(text: String): Node = PCData(text)

  override def comment(text: String): Option[Node] = Some(Comment(text))

  override def processingInstruction(target: String, data: String): Option[Node] = Some(ProcInstr(target, data))

  override def element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Nodes
  ): Element = Elem(
    prefix = name.prefix.filter(_.nonEmpty).orNull,
    label = name.localName,
    attributes = toMetaData(attributes),
    scope = toScope(name, attributes),
    minimizeEmpty = false,
    child = children*
  )

  override def foldNode[A](
    node: Node,
    element: Element => A,
    text: String => A,
    cdata: String => A,
    comment: String => A,
    processingInstruction: (String, String) => A,
    unknown: => A
  ): A = node match
    case elem: Elem => element(elem)
    case value: Text => text(value.data)
    case value: PCData => cdata(value.data)
    case value: Comment => comment(value.commentText)
    case value: ProcInstr => processingInstruction(value.target, value.proctext)
    case _ => unknown

  override def nameOf(element: Element): XmlName = fromScope(
    element.scope,
    element.prefix,
    element.label,
    isAttribute = false
  )

  override def childrenOf(element: Element): Nodes = element.child

  override def attributesOf(element: Element): Seq[(XmlName, String)] = element.attributes.toSeq.map(attribute => (
    fromScope(
      element.scope,
      attribute match
        case prefixed: PrefixedAttribute => prefixed.pre
        case _ => null,
      attribute.key,
      isAttribute = true
    ),
    NodeSeq.fromSeq(attribute.value).text
  ))

  // scala.xml rejects prefix ""; unprefixed names use null.
  private def toMetaData(attributes: Seq[(XmlName, String)]): MetaData = attributes.foldRight(scala.xml.Null: MetaData):
    case ((name, value), next) =>
      Attribute(name.prefix.filter(_.nonEmpty).orNull, name.localName, value, next)

  private def fromScope(
    scope: NamespaceBinding,
    prefix: String,
    local: String,
    isAttribute: Boolean
  ): XmlName =
    val p: Option[String] = Option(prefix).filter(_.nonEmpty)
    val namespace: Option[String] = XmlNamespace.wellKnown(p, local, isAttribute).map(_.uri).orElse:
      if isAttribute && p.isEmpty
      then None
      else Option(scope.getURI(p.orNull)).filter(uri => uri != null && uri.nonEmpty)
    XmlName(local, XmlNamespace.of(p, namespace))

  // `xmlns*` stay attributes so the writer still emits them. Bindings also come
  // from expanded names so a child without its own xmlns keeps the URI.
  private def toScope(
    name: XmlName,
    attributes: Seq[(XmlName, String)]
  ): NamespaceBinding =
    val declared: NamespaceBinding = attributes.foldLeft(xmlScope):
      case (scope, (n, uri)) if n.isDefaultXmlns =>
        NamespaceBinding(null, uri, scope)
      case (scope, (n, uri)) if n.isXmlnsDeclaration =>
        NamespaceBinding(n.localName, uri, scope)
      case (scope, _) => scope
    (name +: attributes.map(_._1)).foldLeft(declared)(bind)

  private val xmlScope: NamespaceBinding = NamespaceBinding(
    XmlNamespace.xml.prefix.orNull,
    XmlNamespace.xml.uri,
    TopScope
  )

  private def bind(
    scope: NamespaceBinding,
    name: XmlName
  ): NamespaceBinding = name.uri match
    case None => scope
    case Some(_) if name.isXmlnsDeclaration => scope
    case Some(uri) =>
      val prefix: String = name.prefix.filter(_.nonEmpty).orNull
      if Option(scope.getURI(prefix)).contains(uri)
      then scope
      else NamespaceBinding(prefix, uri, scope)

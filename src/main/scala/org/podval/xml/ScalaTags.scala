package org.podval.xml

import scala.annotation.unused
import scalatags.Text
import scalatags.generic.{Attr, AttrPair}
import scalatags.text.{Builder, Frag}

// XML AST for ScalaTags (Li Haoyi).
// `import ScalaTags.given` to convert (`element.to[ScalaTags.Element]`).
// Text has no comment, CDATA, or processing instruction. CDATA becomes text.
// `raw` is unparsed markup and is dropped.
// Text has no namespace scope; a URI is kept with an xmlns attribute.
object ScalaTags extends XmlAst[Text.TypedTag[String]]:
  given ScalaTags.type = this

  override type Node = Frag

  private val stringAttr: scalatags.generic.AttrValue[Builder, String] = new Text.GenericAttr[String]

  override def text(text: String): Node = Text.StringFrag(text)

  override def cdata(text: String): Node = this.text(text)

  override def comment(text: String): Option[Node] = None

  override def processingInstruction(target: String, data: String): Option[Node] = None

  override def element(
    name: XmlName,
    attributes: Seq[(XmlName, String)],
    children: Nodes
  ): Element =
    val stored: Seq[(XmlName, String)] = attributes ++ XmlName.xmlnsDeclarations(name, attributes)
    val mods: Seq[Text.Modifier] = stored.map((attr, value) => attribute(attr.qName, value)) ++ children
    Text.TypedTag(tag = name.qName, modifiers = List(mods), void = false)

  override def foldNode[A](
    node: Node,
    element: Element => A,
    text: String => A,
    @unused cdata: String => A,
    @unused comment: String => A,
    @unused processingInstruction: (String, String) => A,
    unknown: => A
  ): A = node match
    case value: Element => element(value)
    case Text.StringFrag(value) => text(value)
    case _ => unknown

  override def nameOf(element: Element): XmlName =
    XmlName.parseDeclared(element.tag, attributesOf(element), isAttribute = false)

  override def childrenOf(element: Element): Nodes =
    val builder: Builder = built(element)
    builder.children.iterator.take(builder.childIndex).toSeq

  override def attributesOf(element: Element): Seq[(XmlName, String)] =
    val builder: Builder = built(element)
    val strings: Seq[(String, String)] = builder.attrs.iterator.take(builder.attrIndex).map { (name, source) =>
      name -> attrValue(source)
    }.toSeq
    strings.map((name, value) => XmlName.parse(name, strings, isAttribute = true) -> value)

  // Scalatags rejects some legal XML names. `raw` skips that check.
  private def attribute(name: String, value: String): Text.Modifier =
    AttrPair(Attr(name, raw = true), value, stringAttr)

  // The builder arrays are the inspectable tree. `attrsString` escapes; read the raw sources.
  private def built(element: Element): Builder =
    val builder: Builder = new Builder()
    element.build(builder)
    builder

  private def attrValue(source: Builder.ValueSource): String = source match
    case Builder.GenericAttrValueSource(value) => value
    case Builder.StyleValueSource(style, value) => s"${style.cssName}: $value;"
    case Builder.ChainedAttributeValueSource(head, tail) => s"${attrValue(head)} ${attrValue(tail)}"
    case other =>
      val writer = new java.io.StringWriter
      other.appendAttrValue(writer)
      writer.toString

package org.podval.xml

import zio.blocks.schema.Schema
import zio.blocks.schema.derive.Deriver
import zio.blocks.schema.xml.Xml as XML
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

/** Identity codec field type. Same as `Xml.Element`.
 *  It has to have Schema instance, so it must be a real type... */
type XmlTree = XML.Element

object XmlTree:
  given schema: Schema[XmlTree] =
    Schema[Unit].transform(_ => XML.Element.empty, _ => ())(using TypeId.of[XML.Element])

/** Same as [[XmlCodec.xmlElementSchema]]. In this package automatically. */
given xmlElementSchema: Schema[XML.Element] = XmlTree.schema

/** Document-shaped XML codec over any `XmlAst`.
  *
  * Derive with `XmlCodec.derived` from a `Schema`. Binding hints are Schema modifiers
  * (`Modifier` is sealed, so `@xmlAttribute` is not visible to `Schema.derived`):
  *
  * {{{
  * final case class Language(
  *   @Modifier.config(XmlCodec.Attribute, "") ident: String
  * )
  * given Schema[Language] = Schema.derived
  * val codec: XmlCodec[Language] = XmlCodec.derived
  * val el: Xml.Element = codec.encode(Language("ru"))
  * }}}
  *
  * Identity fields are `Xml.Element` (alias [[XmlTree]]). The child tag is the field
  * name unless `@Modifier.config(XmlCodec.Element, …)` overrides it. `encode` is
  * polymorphic in the AST; pin it with a type ascription when more than one `XmlAst`
  * is in scope. Other packages `import XmlCodec.given` (or `import org.podval.xml.given`)
  * for `Schema[Xml.Element]`.
  *
  * `@Modifier.config(XmlCodec.IgnoreUnknown, "")` on a record skips leftover
  * attributes, elements, and character content. `@Modifier.config(XmlCodec.Include, "")`
  * on a `Seq[String]` gathers `xi:include/@href` from the subtree.
  */
object XmlCodec:
  /** `@Modifier.config(XmlCodec.Attribute, "")` or `@Modifier.config(XmlCodec.Attribute, "xml:id")`. */
  final val Attribute = "xml.attribute"
  /** Type or field element name: `@Modifier.config(XmlCodec.Element, "persName")`. */
  final val Element = "xml.element"
  /** Character content of this element: `@Modifier.config(XmlCodec.Text, "")`. */
  final val Text = "xml.text"
  final val NamespaceUri = "xml.namespace.uri"
  final val NamespacePrefix = "xml.namespace.prefix"
  /** Record: do not fail on leftover attributes, elements, or character content. */
  final val IgnoreUnknown = "xml.ignoreUnknown"
  /** `Seq[String]` field: `xi:include/@href` from this element and descendants. */
  final val Include = "xml.include"

  /** Identity field `Schema`. `import XmlCodec.given` when deriving outside this package. */
  given xmlElementSchema: Schema[XML.Element] = XmlTree.schema

  /** Identity field: copy a named child as canonical ZIO XML. Same-AST decode keeps the node. */
  val elementCodec: XmlCodec[XmlTree] = new XmlCodec[XmlTree]:
    override def elementName: String = "element"
    override def isRecordLike: Boolean = true
    override def isIdentity: Boolean = true
    override def unsafeDecode[E: XmlAst](element: E): XmlTree = toZioElement(element)
    override def encodeNamed[E: XmlAst](name: String, value: XmlTree): E = fromZioElement(Xml.withName(value, name))
    override def encode[E: XmlAst](value: XmlTree): E = fromZioElement(value)

  private def toZioElement[E: XmlAst](element: E): XmlTree =
    if summon[XmlAst[E]] eq Xml then element.asInstanceOf[XmlTree]
    else element.to[XML.Element]

  private def fromZioElement[E: XmlAst](element: XmlTree): E =
    if summon[XmlAst[E]] eq Xml then element.asInstanceOf[E]
    else element.to[E]

  val deriver: Deriver[XmlCodec] = XmlCodecDeriver

  def derived[A](using schema: Schema[A]): XmlCodec[A] = schema.derive(deriver)

  /** Like `derived`, after nested tagged codecs have been initialized.
    * `Schema.derived` inlines nested records and does not run `Child.codec`. */
  def derived[A](nested: XmlCodec[?], rest: XmlCodec[?]*)(using schema: Schema[A]): XmlCodec[A] =
    derived(using schema)

  /** Derive a record whose XML tag comes from `tagField` via `tag`.
    * Nested records of this type in a later `XmlCodec.derived` pick up the tagged codec
    * once this method has run (pass `Child.codec` to `XmlCodec.derived(nested)`). */
  def derived[A, K](tagField: String, tag: XmlTag[K])(using schema: Schema[A], typeId: TypeId[A]): XmlCodec[A] =
    val codec: XmlCodec[A] = schema.derive(XmlCodecDeriver.tagged(tagField, tag))
    XmlCodecDeriver.registerTagged(typeId, codec)
    codec

  extension [A](codec: XmlCodec[A])
    /** Decode each element child of `root`. Whitespace and comments are ignored;
      * leftover character content is an error. */
    def decodeChildren[E: XmlAst](root: E): Either[XmlError, Seq[A]] =
      val ast: XmlAst[E] = summon[XmlAst[E]]
      val nodes: ast.Nodes = ast.getChildren(root)
      val leftover: Seq[String] = nodes
        .filter(_.asElement.isEmpty)
        .flatMap(_.asAtom)
        .map(_.trim)
        .filter(_.nonEmpty)
      if leftover.nonEmpty then Left(XmlError(s"Unparsed characters: ${leftover.mkString}"))
      else
        nodes.flatMap(_.asElement).foldLeft(Right(Vector.empty[A]): Either[XmlError, Vector[A]]): (acc, element) =>
          for
            items <- acc
            item <- codec.decode(element)
          yield items :+ item
        .map(_.toSeq)

    /** Require wrapper element `name`, then [[decodeChildren]].
      * `XmlParser.loadCatalog` uses this after loading `name.xml`. */
    def decodeCatalog[E: XmlAst](root: E, name: String): Either[XmlError, Seq[A]] =
      if !root.isNamed(name) then
        Left(XmlError(s"Expected catalog '$name', found '${root.getName.qName}'"))
      else codec.decodeChildren(root)

trait XmlCodec[A]:
  def elementName: String

  def decode[E: XmlAst](element: E): Either[XmlError, A] =
    try Right(unsafeDecode(element))
    catch
      case e: XmlError => Left(e)
      case e if NonFatal(e) => Left(XmlError(Option(e.getMessage).getOrElse(e.toString), e))

  def elementNameOf(value: A): String = elementName

  def encode[E: XmlAst](value: A): E = encodeNamed(elementNameOf(value), value)

  def encodeNamed[E: XmlAst](name: String, value: A): E

  def unsafeDecode[E: XmlAst](element: E): A

  def unsafeDecodeText(text: String): A =
    throw XmlError("Type does not decode from text")

  def encodeText(value: A): String =
    throw XmlError("Type does not encode as text")

  def caseNames: Seq[String] = Seq.empty

  def isEnumeration: Boolean = false

  /** Nested record/identity: child name comes from the type (or an override), not from a primitive wrapper. */
  def isRecordLike: Boolean = false

  /** Identity `Xml.Element` field: child tag is the Scala field name unless overridden. */
  def isIdentity: Boolean = false

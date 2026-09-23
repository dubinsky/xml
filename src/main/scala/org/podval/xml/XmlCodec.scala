package org.podval.xml

import zio.blocks.schema.Schema
import zio.blocks.schema.derive.Deriver
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

/** Identity codec field type. Same as `Xml.Element`.
 *  It has to have a Schema instance, so it must be a real type.
 *  The schema discards the tree: [[XmlCodec]] carries the XML, not `Schema`.
 */
type XmlTree = Xml.Element

object XmlTree:
  given schema: Schema[XmlTree] =
    Schema[Unit].transform(_ => Xml.element("empty"), _ => ())(using TypeId.of[XmlNode.Element])

/** Same as [[XmlCodec.xmlElementSchema]]. In this package automatically. */
given xmlElementSchema: Schema[Xml.Element] = XmlTree.schema

/** Document-shaped XML codec for [[Xml.Element]].
  *
  * Derive with `XmlCodec.derived` from a `Schema`. Binding hints are Schema modifiers
  * (`Modifier` is sealed, so `@xmlAttribute` is not visible to `Schema.derived`):
  *
  * {{{
  * final case class Language(ident: String)
  * given Schema[Language] = Schema.derived
  * val codec: XmlCodec[Language] = XmlCodec.derived
  * val el: Xml.Element = codec.encode(Language("ru"))
  * }}}
  *
  * An unannotated primitive is an attribute named after the field.
  * `@Modifier.rename("n")` renames that attribute, or the child tag of a record or identity field.
  * `@Modifier.config(XmlCodec.Element, "comment")` forces a primitive into a child element.
  * Put that config on the case class when the type has one XML tag.
  * Identity fields are `Xml.Element` (alias [[XmlTree]]). The child tag is the field
  * name unless rename or an element config overrides it.
  * A foreign tree decodes after `element.to[Xml.Element]`; encode with `codec.encode(value).to[TO]`.
  * Other packages `import XmlCodec.given` (or `import org.podval.xml.given`)
  * for `Schema[Xml.Element]`.
  *
  * `@Modifier.config(XmlCodec.IgnoreUnknown, "")` on a record skips leftover
  * attributes, elements, and character content. `@Modifier.config(XmlCodec.Include, "")`
  * on a `Seq[String]` gathers `xi:include/@href` from the subtree.
  */
object XmlCodec:
  /** `@Modifier.config` value with no extra XML name.
    * On `Attribute`, this is the Scala field name.
    * On `Text`, `Include`, and `IgnoreUnknown` it is the required dummy string.
    */
  final val UseFieldName: String = ""

  /** Attribute qName, or [[UseFieldName]] for `@Modifier.rename` or the field name.
    * A non-empty value (`"xml:id"`) wins over rename.
    * An unannotated primitive is already an attribute of that name.
    * `@Modifier.rename` is enough to rename it.
    */
  final val Attribute = "xml.attribute"
  /** Element name on a type, or the child tag that forces a primitive into an element.
    * `@Modifier.config(XmlCodec.Element, "persName")`.
    * A record sequence renamed to a tag it already is uses `@Modifier.rename`.
    */
  final val Element = "xml.element"
  /** Character content of this element: `@Modifier.config(XmlCodec.Text, UseFieldName)`. */
  final val Text = "xml.text"
  final val NamespaceUri = "xml.namespace.uri"
  final val NamespacePrefix = "xml.namespace.prefix"
  /** Record: do not fail on leftover attributes, elements, or character content. */
  final val IgnoreUnknown = "xml.ignoreUnknown"
  /** `Seq[String]` field: `xi:include/@href` from this element and descendants. */
  final val Include = "xml.include"

  /** Identity field `Schema`. `import XmlCodec.given` when deriving outside this package. */
  given xmlElementSchema: Schema[Xml.Element] = XmlTree.schema

  /** Identity field: copy a named child as [[Xml.Element]]. */
  val elementCodec: XmlCodec[XmlTree] = new XmlCodec[XmlTree]:
    override def elementName: String = "element"
    override def isRecordLike: Boolean = true
    override def isIdentity: Boolean = true
    override def unsafeDecode(element: Xml.Element): XmlTree = element
    override def encodeNamed(name: String, value: XmlTree): Xml.Element = Xml.withName(value, name)
    override def encode(value: XmlTree): Xml.Element = value

  val deriver: Deriver[XmlCodec] = XmlCodecDeriver

  def derived[A](using schema: Schema[A]): XmlCodec[A] = schema.derive(deriver)

  /** The only supported way to publish a tagged child codec into a parent `derived`.
    *
    * The parameters are unused in the body on purpose.
    * The call publishes the child codec because the argument expressions run first:
    * `derived(tagField, tag)` calls `registerTagged`, which fills `instanceOverrides`,
    * before this method derives the parent.
    * `Schema.derived` inlines nested records and does not run the child codec.
    */
  def derived[A](nested: XmlCodec[?], rest: XmlCodec[?]*)(using schema: Schema[A]): XmlCodec[A] =
    val _ = (nested, rest)
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
    def decodeChildren(root: Xml.Element): Either[XmlError, Seq[A]] =
      val nodes: Seq[XmlNode] = root.getChildren
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
    def decodeCatalog(root: Xml.Element, name: String): Either[XmlError, Seq[A]] =
      if !root.isNamed(name) then
        Left(XmlError(s"Expected catalog '$name', found '${root.getName.qName}'"))
      else codec.decodeChildren(root)

trait XmlCodec[A]:
  def elementName: String

  def decode(element: Xml.Element): Either[XmlError, A] =
    try Right(unsafeDecode(element))
    catch
      case e: XmlError => Left(e)
      case e if NonFatal(e) => Left(XmlError(Option(e.getMessage).getOrElse(e.toString), e))

  def elementNameOf(value: A): String = elementName

  def encode(value: A): Xml.Element = encodeNamed(elementNameOf(value), value)

  def encodeNamed(name: String, value: A): Xml.Element

  def unsafeDecode(element: Xml.Element): A

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

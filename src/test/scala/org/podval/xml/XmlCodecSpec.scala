package org.podval.xml

import Xml.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.schema.{Modifier, Schema}

final class XmlCodecSpec extends AnyFunSuite:
  private def parse(xml: String): Xml.Element = XmlParser.parseXml(xml).toOption.get

  test("leaf record keeps attributes") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val language: Language = codec.decode(parse("""<Language ident="ru"/>""")).toOption.get
    assert(language.ident == "ru")
    val encoded: Xml.Element = codec.encode(language)
    assert(encoded.get("ident").contains("ru"))
    assert(encoded.childElements.isEmpty)
  }

  test("unwrapped sibling sequences") {
    val codec: XmlCodec[LangUsage] = XmlCodec.derived(using LangUsage.schema)
    val xml: String = """<LangUsage><language ident="ru"/><language ident="he"/></LangUsage>"""
    val decoded: LangUsage = codec.decode(parse(xml)).toOption.get
    assert(decoded.languages.map(_.ident) == Seq("ru", "he"))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.childElements.map(_.getName.qName) == Seq("language", "language"))
    assert(encoded.childElements.flatMap(_.get("ident")) == Seq("ru", "he"))
  }

  test("text plus attributes on the same element") {
    val codec: XmlCodec[EntityName] = XmlCodec.derived(using EntityName.schema)
    val decoded: EntityName = codec.decode(parse("""<EntityName id="n1">Moses</EntityName>""")).toOption.get
    assert(decoded.id.contains("n1"))
    assert(decoded.ref.isEmpty)
    assert(decoded.name == "Moses")
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.get(XmlAttribute.Id).contains("n1"))
    assert(encoded.getText.trim == "Moses")
  }

  test("boolean decodes true/false/yes/no/1/0 and encodes true/false") {
    val codec: XmlCodec[Flag] = XmlCodec.derived(using Flag.schema)
    assert(codec.decode(parse("""<Flag on="yes"/>""")).toOption.get.on)
    assert(codec.decode(parse("""<Flag on="1"/>""")).toOption.get.on)
    assert(!codec.decode(parse("""<Flag on="no"/>""")).toOption.get.on)
    assert(!codec.decode(parse("""<Flag on="0"/>""")).toOption.get.on)
    val encodedTrue: Xml.Element = codec.encode(Flag(true))
    val encodedFalse: Xml.Element = codec.encode(Flag(false))
    assert(encodedTrue.get("on").contains("true"))
    assert(encodedFalse.get("on").contains("false"))
  }

  test("invalid int attribute wraps NumberFormatException") {
    val codec: XmlCodec[Count] = XmlCodec.derived(using Count.schema)
    val err: XmlError = codec.decode(parse("""<Count n="x"/>""")).swap.toOption.get
    assert(err.getMessage.contains("Invalid"))
    assert(err.getCause != null)
  }

  test("unparsed children are an error") {
    val codec: XmlCodec[Box] = XmlCodec.derived(using Box.schema)
    val result: Either[XmlError, Box] = codec.decode(parse("""<Box n="1"><extra/></Box>"""))
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Unparsed elements"))
  }

  test("unparsed attributes are an error") {
    val codec: XmlCodec[Box] = XmlCodec.derived(using Box.schema)
    val result: Either[XmlError, Box] = codec.decode(parse("""<Box n="1" role="x"/>"""))
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Unparsed attributes"))
  }

  test("identity Xml.Element round-trips mixed content; child tag is the field name") {
    val codec: XmlCodec[Text] = XmlCodec.derived(using Text.schema)
    val xml: String = """<Text lang="ru"><body><!--n--><p>a<hi>b</hi></p></body></Text>"""
    val decoded: Text = codec.decode(parse(xml)).toOption.get
    assert(decoded.lang.contains("ru"))
    assert(decoded.body.isElement(XmlElement.Body))
    assert(decoded.body.getChildren.exists(_.asComment.contains("n")))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.get(XmlAttribute.Lang).contains("ru"))
    assert(encoded.childElements.map(_.getName.qName) == Seq("body"))
    assert(encoded.childElements.head.getChildren.exists(_.asComment.contains("n")))
    val scalaEl: ScalaXml.Element = codec.encode(decoded).to[ScalaXml.Element]
    assert(scalaEl.getName.qName == "Text")
    assert(codec.decode(ScalaXml.converted[Xml.Element](scalaEl)).toOption.get.body.childElements.map(_.getName.qName) == Seq("p"))
  }

  test("IgnoreUnknown skips leftover attributes, elements, and text") {
    val codec: XmlCodec[OpenDoc] = XmlCodec.derived(using OpenDoc.schema)
    val xml: String =
      """<OpenDoc n="1" extra="x"><title>Go</title><noise>z</noise> leftover <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="a.xml"/></OpenDoc>"""
    val decoded: OpenDoc = codec.decode(parse(xml)).toOption.get
    assert(decoded.n == "1")
    assert(decoded.title.exists(_.getText.contains("Go")))
    assert(decoded.hrefs == Seq("a.xml"))
  }

  test("Include gathers nested xi:include hrefs") {
    val codec: XmlCodec[OpenDoc] = XmlCodec.derived(using OpenDoc.schema)
    val xml: String =
      """<OpenDoc n="1"><by><xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="nested.xml"/></by></OpenDoc>"""
    val decoded: OpenDoc = codec.decode(parse(xml)).toOption.get
    assert(decoded.hrefs == Seq("nested.xml"))
  }

  test("nested tagged codec is used without TypeId.instance") {
    assert(TaggedItem.codec.caseNames == Seq("person", "place"))
    val xml: String = """<TaggedIndex><person n="a"/><place n="b"/></TaggedIndex>"""
    val result: Either[XmlError, TaggedIndex] = TaggedIndex.codec.decode(parse(xml))
    assert(result.isRight, result.swap.toOption.map(_.getMessage).getOrElse(""))
    val decoded: TaggedIndex = result.toOption.get
    assert(decoded.items.map(_.n) == Seq("a", "b"))
    assert(decoded.items.map(_.kind) == Seq(TaggedKind.person, TaggedKind.place))
    val encoded: Xml.Element = TaggedIndex.codec.encode(decoded)
    assert(encoded.childElements.map(_.getName.qName) == Seq("person", "place"))
    val plain: Xml.Element = PlainIndex.codec.encode(PlainIndex(Seq(TaggedItem(TaggedKind.person, "a"))))
    assert(plain.childElements.map(_.getName.qName) == Seq("TaggedItem"))
  }

  test("Schema[Xml.Element] does not carry the tree") {
    val failed: XmlError = intercept[XmlError] {
      summon[Schema[Xml.Element]].toDynamicValue(Xml.element("p"))
    }
    assert(failed.getMessage.contains("use XmlCodec"))
  }

  test("identity sequence fields collect mixed sibling tags") {
    val codec: XmlCodec[DayDoc] = XmlCodec.derived(using DayDoc.schema)
    val xml: String =
      """<DayDoc n="x"><torah id="t"/><haftarah id="h1"/><maftir id="m"/><haftarah id="h2"/></DayDoc>"""
    val decoded: DayDoc = codec.decode(parse(xml)).toOption.get
    assert(decoded.n == "x")
    assert(decoded.torah.flatMap(_.getId) == Seq("t"))
    assert(decoded.maftir.flatMap(_.getId) == Seq("m"))
    assert(decoded.haftarah.flatMap(_.getId) == Seq("h1", "h2"))
  }

  test("class element config names a nested record without passing its codec") {
    val encoded: Xml.Element = InlinedBook.codec.encode(InlinedBook(Seq(InlinedChapter(2), InlinedChapter(3))))
    assert(encoded.childElements.map(_.getName.qName) == Seq("chapter", "chapter"))
    assert(encoded.childElements.flatMap(_.get("n")) == Seq("2", "3"))
    val decoded: InlinedBook = InlinedBook.codec.decode(parse(
      """<InlinedBook><chapter n="2"/><chapter n="3"/></InlinedBook>"""
    )).toOption.get
    assert(decoded == InlinedBook(Seq(InlinedChapter(2), InlinedChapter(3))))
  }

  test("derived(element) names that derivation and wins over the class annotation") {
    assert(RenamedRoot.codec.encode(RenamedRoot(1)).getName.qName == "root")
    assert(RenamedRoot.codec.decode(parse("""<root n="1"/>""")).toOption.get == RenamedRoot(1))
  }

  test("sealed trait sequence uses case element names") {
    val codec: XmlCodec[Lesson] = XmlCodec.derived(using Lesson.schema)
    val xml: String = """<Lesson><positive n="1"/><negative n="2"/></Lesson>"""
    val decoded: Lesson = codec.decode(parse(xml)).toOption.get
    assert(decoded.parts == Seq(Positive(1), Negative(2)))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.childElements.map(_.getName.qName) == Seq("positive", "negative"))
  }

  test("encode converts to Scala XML") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val zioEl: Xml.Element = codec.encode(Language("he"))
    val scalaEl: ScalaXml.Element = zioEl.to[ScalaXml.Element]
    assert(zioEl.get("ident").contains("he"))
    val round: Xml.Element = ScalaXml.converted[Xml.Element](scalaEl)
    assert(round.get("ident").contains("he"))
    assert(codec.decode(zioEl).toOption.get == Language("he"))
    assert(codec.decode(round).toOption.get == Language("he"))
  }

  test("prefixed attribute names") {
    val codec: XmlCodec[Named] = XmlCodec.derived(using Named.schema)
    val decoded: Named = codec.decode(parse("""<Named xml:id="x">Ada</Named>""")).toOption.get
    assert(decoded.id.contains("x"))
    assert(decoded.name == "Ada")
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.get(XmlAttribute.XmlId).contains("x"))
    assert(encoded.attributes.find(_._1.qName == "xml:id").get._1.uri.contains(XmlNamespace.xml.uri))
  }

  test("leftover xmlns is not an error") {
    val codec: XmlCodec[Box] = XmlCodec.derived(using Box.schema)
    val decoded: Box = codec.decode(parse("""<Box xmlns:ex="http://example.com/ns" n="1"/>""")).toOption.get
    assert(decoded.n == "1")
  }

  test("namespace modifiers encode xmlns and a prefixed name") {
    val codec: XmlCodec[NsBox] = XmlCodec.derived(using NsBox.schema)
    val encoded: Xml.Element = codec.encode(NsBox("1"))
    assert(encoded.getName.qName == "ex:NsBox")
    assert(encoded.name.localName == "NsBox")
    assert(encoded.name.prefix.contains("ex"))
    assert(encoded.name.uri.contains("http://example.com/ns"))
    assert(encoded.get(XmlAttribute.Xmlns("ex")).contains("http://example.com/ns"))
    assert(encoded.get("n").contains("1"))
    val decoded: NsBox = codec.decode(parse("""<ex:NsBox xmlns:ex="http://example.com/ns" n="1"/>""")).toOption.get
    assert(decoded.n == "1")
    val scalaEl: ScalaXml.Element = codec.encode(NsBox("1")).to[ScalaXml.Element]
    assert(scalaEl.getName.qName == "ex:NsBox")
    assert(scalaEl.scope.getURI("ex") == "http://example.com/ns")
  }

  test("optional child is absent when missing") {
    val codec: XmlCodec[Book] = XmlCodec.derived(using Book.schema)
    assert(codec.decode(parse("<Book/>")).toOption.get.title.isEmpty)
    val withTitle: Book = codec.decode(parse("<Book><Title>Go</Title></Book>")).toOption.get
    assert(withTitle.title.map(_.value).contains("Go"))
  }

  test("decodeCatalog reads wrapper children") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val xml: String = """<Languages>
      |  <Language ident="ru"/>
      |  <Language ident="he"/>
      |</Languages>""".stripMargin
    val decoded: Seq[Language] = codec.decodeCatalog(parse(xml), "Languages").toOption.get
    assert(decoded.map(_.ident) == Seq("ru", "he"))
  }

  test("decodeCatalog rejects the wrong wrapper name") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val result: Either[XmlError, Seq[Language]] =
      codec.decodeCatalog(parse("""<No><Language ident="ru"/></No>"""), "Languages")
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Expected catalog 'Languages'"))
  }

  test("decodeCatalog fails on a child that does not decode") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val result: Either[XmlError, Seq[Language]] =
      codec.decodeCatalog(parse("""<Languages><Language ident="ru"/><extra/></Languages>"""), "Languages")
    assert(result.isLeft)
  }

  test("className strips a trailing $") {
    assert(XmlParser.className(classOf[XmlCodecSpec]) == "XmlCodecSpec")
    assert(XmlParser.className(XmlParser.getClass) == "XmlParser")
  }

  test("classpath catalog decodes children of the wrapper") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val root: Xml.Element =
      XmlParser.parseResource(classOf[XmlCodecSpec], "languages.xml").toOption.get
    val decoded: Seq[Language] = codec.decodeCatalog(root, "Languages").toOption.get
    assert(decoded.map(_.ident) == Seq("ru", "he"))
  }

  test("nested recursive children round-trip") {
    val codec: XmlCodec[Node] = Node.codec
    val xml: String = """<node n="root"><node n="a"/><node n="b"><node n="c"/></node></node>"""
    val decoded: Node = codec.decode(parse(xml)).toOption.get
    assert(decoded == Node("root", Seq(Node("a"), Node("b", Seq(Node("c"))))))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(codec.decode(encoded).toOption.get == decoded)
  }

  test("unannotated primitives are attributes; element, identity, and sequence stay children") {
    val value: AttrDefault = AttrDefault(
      label = "t",
      count = Some(3),
      comment = Some("c"),
      note = Some(Xml.element("p")),
      tags = Seq("a", "b")
    )
    val encoded: Xml.Element = XmlCodec.derived(using AttrDefault.schema).encode(value)
    assert(encoded.get("label").contains("t"))
    assert(encoded.get("count").contains("3"))
    assert(encoded.get("comment").isEmpty)
    assert(encoded.get("note").isEmpty)
    assert(encoded.get("tags").isEmpty)
    val children: Seq[Xml.Element] = encoded.childElements
    assert(children.map(_.getName.qName) == Seq("comment", "note", "tags", "tags"))
    assert(children.head.getText.trim == "c")
    assert(children.drop(2).map(_.getText.trim) == Seq("a", "b"))

    val missing: Either[XmlError, RequiredLabel] =
      XmlCodec.derived(using RequiredLabel.schema).decode(parse("<RequiredLabel/>"))
    val message: String = missing.swap.toOption.get.getMessage
    assert(message.contains("label"))
    assert(message.contains("Missing required attribute"))

    val renamed: Xml.Element = XmlCodec.derived(using RenamedLabel.schema).encode(RenamedLabel("t"))
    assert(renamed.get("n").contains("t"))
    assert(renamed.get("label").isEmpty)
    assert(renamed.childElements.isEmpty)

    val explicit: Xml.Element =
      XmlCodec.derived(using ExplicitQName.schema).encode(ExplicitQName("collection"))
    assert(explicit.get("pageType").contains("collection"))
    assert(explicit.get("renamed").isEmpty)
    assert(explicit.get("pageTypeName").isEmpty)
  }

  test("transient is not read or written; encodeTransient is read and not written") {
    val codec: XmlCodec[TransientRow] = XmlCodec.derived(using TransientRow.schema)
    val decoded: TransientRow = codec.decode(parse("""<TransientRow n="1" note="x"/>""")).toOption.get
    assert(decoded.n == 1)
    assert(!decoded.isCollection)
    assert(decoded.note.contains("x"))
    val encoded: Xml.Element = codec.encode(decoded.copy(isCollection = true, note = Some("kept")))
    assert(encoded.get("n").contains("1"))
    assert(encoded.get("isCollection").isEmpty)
    assert(encoded.get("note").isEmpty)
    assert(encoded.childElements.isEmpty)

    val stray: Either[XmlError, TransientRow] =
      codec.decode(parse("""<TransientRow n="1" isCollection="true"/>"""))
    assert(stray.swap.toOption.get.getMessage.contains("isCollection"))
  }

  test("unknown config key fails derivation") {
    val error: XmlError = intercept[XmlError] {
      val codec: XmlCodec[BadConfig] = XmlCodec.derived(using BadConfig.schema)
      codec.encode(BadConfig("x"))
    }
    assert(error.getMessage.contains("xml.nope"), error.getMessage)
  }

final case class Language(
  ident: String
) derives CanEqual
object Language:
  given schema: Schema[Language] = Schema.derived

final case class LangUsage(
  @Modifier.config(XmlCodec.Element, "language") languages: Seq[Language]
) derives CanEqual
object LangUsage:
  given schema: Schema[LangUsage] = Schema.derived

final case class EntityName(
  id: Option[String] = None,
  ref: Option[String] = None,
  @Modifier.config(XmlCodec.Text, "") name: String
) derives CanEqual
object EntityName:
  given schema: Schema[EntityName] = Schema.derived

final case class Flag(
  on: Boolean
) derives CanEqual
object Flag:
  given schema: Schema[Flag] = Schema.derived

final case class Box(
  n: String
) derives CanEqual
object Box:
  given schema: Schema[Box] = Schema.derived

final case class Count(
  n: Int
) derives CanEqual
object Count:
  given schema: Schema[Count] = Schema.derived

final case class Text(
  lang: Option[String],
  body: Xml.Element
) derives CanEqual
object Text:
  given schema: Schema[Text] = Schema.derived

sealed trait Part derives CanEqual
@Modifier.config(XmlCodec.Element, "positive")
final case class Positive(n: Int) extends Part derives CanEqual
@Modifier.config(XmlCodec.Element, "negative")
final case class Negative(n: Int) extends Part derives CanEqual
object Part:
  given schema: Schema[Part] = Schema.derived

final case class Lesson(parts: Seq[Part]) derives CanEqual
object Lesson:
  given schema: Schema[Lesson] = Schema.derived

@Modifier.config(XmlCodec.Element, "chapter")
final case class InlinedChapter(n: Int) derives CanEqual
object InlinedChapter:
  given schema: Schema[InlinedChapter] = Schema.derived

final case class InlinedBook(chapters: Seq[InlinedChapter]) derives CanEqual
object InlinedBook:
  given schema: Schema[InlinedBook] = Schema.derived
  val codec: XmlCodec[InlinedBook] = XmlCodec.derived

@Modifier.config(XmlCodec.Element, "ignored")
final case class RenamedRoot(n: Int) derives CanEqual
object RenamedRoot:
  given schema: Schema[RenamedRoot] = Schema.derived
  val codec: XmlCodec[RenamedRoot] = XmlCodec.derived(element = "root")

final case class Named(
  @Modifier.config(XmlCodec.Attribute, "xml:id") id: Option[String],
  @Modifier.config(XmlCodec.Text, "") name: String
) derives CanEqual
object Named:
  given schema: Schema[Named] = Schema.derived

@Modifier.config(XmlCodec.NamespaceUri, "http://example.com/ns")
@Modifier.config(XmlCodec.NamespacePrefix, "ex")
final case class NsBox(
  n: String
) derives CanEqual
object NsBox:
  given schema: Schema[NsBox] = Schema.derived

final case class Title(@Modifier.config(XmlCodec.Text, "") value: String) derives CanEqual
object Title:
  given schema: Schema[Title] = Schema.derived

final case class Book(title: Option[Title]) derives CanEqual
object Book:
  given schema: Schema[Book] = Schema.derived

@Modifier.config(XmlCodec.IgnoreUnknown, "")
final case class OpenDoc(
  n: String,
  title: Option[Xml.Element] = None,
  @Modifier.config(XmlCodec.Include, "") hrefs: Seq[String] = Seq.empty
) derives CanEqual
object OpenDoc:
  given schema: Schema[OpenDoc] = Schema.derived

enum TaggedKind derives CanEqual:
  case person, place
object TaggedKind:
  given schema: Schema[TaggedKind] = Schema.derived
  val asList: XmlTag[TaggedKind] = XmlTag(
    _.toString,
    name => TaggedKind.values.find(_.toString == name),
    TaggedKind.values.map(_.toString).toSeq
  )

final case class TaggedItem(
  kind: TaggedKind,
  n: String
) derives CanEqual
object TaggedItem:
  given schema: Schema[TaggedItem] = Schema.derived
  val codec: XmlCodec[TaggedItem] = XmlCodec.derived[TaggedItem, TaggedKind]("kind", TaggedKind.asList)

final case class TaggedIndex(items: Seq[TaggedItem] = Seq.empty) derives CanEqual
object TaggedIndex:
  given schema: Schema[TaggedIndex] = Schema.derived
  val codec: XmlCodec[TaggedIndex] = XmlCodec.derived(TaggedItem.codec)

@Modifier.config(XmlCodec.IgnoreUnknown, "")
final case class DayDoc(
  n: String,
  torah: Seq[Xml.Element] = Seq.empty,
  maftir: Seq[Xml.Element] = Seq.empty,
  haftarah: Seq[Xml.Element] = Seq.empty
) derives CanEqual
object DayDoc:
  given schema: Schema[DayDoc] = Schema.derived

final case class Node(
  n: String,
  children: Seq[Node] = Seq.empty
) derives CanEqual
object Node:
  given schema: Schema[Node] = Schema.derived
  val codec: XmlCodec[Node] = XmlCodec.derived(element = "node")

final case class PlainIndex(items: Seq[TaggedItem] = Seq.empty) derives CanEqual
object PlainIndex:
  given schema: Schema[PlainIndex] = Schema.derived
  val codec: XmlCodec[PlainIndex] = XmlCodec.derived

final case class AttrDefault(
  label: String,
  count: Option[Int],
  @Modifier.config(XmlCodec.Element, "comment") comment: Option[String],
  note: Option[Xml.Element],
  tags: Seq[String]
) derives CanEqual
object AttrDefault:
  given schema: Schema[AttrDefault] = Schema.derived

final case class RequiredLabel(label: String) derives CanEqual
object RequiredLabel:
  given schema: Schema[RequiredLabel] = Schema.derived

final case class RenamedLabel(@Modifier.rename("n") label: String) derives CanEqual
object RenamedLabel:
  given schema: Schema[RenamedLabel] = Schema.derived

final case class TransientRow(
  n: Int,
  @Modifier.transient() isCollection: Boolean = false,
  @Modifier.encodeTransient() note: Option[String] = None
) derives CanEqual
object TransientRow:
  given schema: Schema[TransientRow] = Schema.derived

final case class BadConfig(
  @Modifier.config("xml.nope", "") n: String
) derives CanEqual
object BadConfig:
  given schema: Schema[BadConfig] = Schema.derived

final case class ExplicitQName(
  @Modifier.config(XmlCodec.Attribute, "pageType")
  @Modifier.rename("renamed")
  pageTypeName: String
) derives CanEqual
object ExplicitQName:
  given schema: Schema[ExplicitQName] = Schema.derived

package org.podval.xml

import org.podval.xml.scalaxml.given
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
    assert(encoded.getChildren.flatMap(_.asElement).isEmpty)
  }

  test("unwrapped sibling sequences") {
    val codec: XmlCodec[LangUsage] = XmlCodec.derived(using LangUsage.schema)
    val xml: String = """<LangUsage><language ident="ru"/><language ident="he"/></LangUsage>"""
    val decoded: LangUsage = codec.decode(parse(xml)).toOption.get
    assert(decoded.languages.map(_.ident) == Seq("ru", "he"))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("language", "language"))
    assert(encoded.getChildren.flatMap(_.asElement).flatMap(_.get("ident")) == Seq("ru", "he"))
  }

  test("text plus attributes on the same element") {
    val codec: XmlCodec[EntityName] = XmlCodec.derived(using EntityName.schema)
    val decoded: EntityName = codec.decode(parse("""<EntityName id="n1">Moses</EntityName>""")).toOption.get
    assert(decoded.id.contains("n1"))
    assert(decoded.ref.isEmpty)
    assert(decoded.name == "Moses")
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.get("id").contains("n1"))
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

  test("identity Xml.Element round-trips mixed content") {
    val codec: XmlCodec[Text] = XmlCodec.derived(using Text.schema)
    val xml: String = """<Text lang="ru"><body><!--n--><p>a<hi>b</hi></p></body></Text>"""
    val decoded: Text = codec.decode(parse(xml)).toOption.get
    assert(decoded.lang.contains("ru"))
    assert(decoded.body.getName == "body")
    assert(decoded.body.getChildren.exists(_.asComment.contains("n")))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.get("lang").contains("ru"))
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("body"))
    assert(encoded.getChildren.flatMap(_.asElement).head.getChildren.exists(_.asComment.contains("n")))
    val scalaEl: ScalaXml.Element = codec.encode(decoded)
    assert(ScalaXml.getName(scalaEl) == "Text")
    assert(codec.decode(scalaEl).toOption.get.body.getChildren.flatMap(_.asElement).map(_.getName) == Seq("p"))
  }

  test("sealed trait sequence uses case element names") {
    val codec: XmlCodec[Lesson] = XmlCodec.derived(using Lesson.schema)
    val xml: String = """<Lesson><positive n="1"/><negative n="2"/></Lesson>"""
    val decoded: Lesson = codec.decode(parse(xml)).toOption.get
    assert(decoded.parts == Seq(Positive(1), Negative(2)))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.getChildren.flatMap(_.asElement).map(_.getName) == Seq("positive", "negative"))
  }

  test("the same codec encodes ZIO XML and Scala XML") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val zioEl: Xml.Element = codec.encode(Language("he"))
    val scalaEl: ScalaXml.Element = codec.encode(Language("he"))
    assert(zioEl.get("ident").contains("he"))
    assert(scalaEl.get("ident").contains("he"))
    assert(codec.decode(zioEl).toOption.get == Language("he"))
    assert(codec.decode(scalaEl).toOption.get == Language("he"))
  }

  test("prefixed attribute names") {
    val codec: XmlCodec[Named] = XmlCodec.derived(using Named.schema)
    val decoded: Named = codec.decode(parse("""<Named xml:id="x">Ada</Named>""")).toOption.get
    assert(decoded.id.contains("x"))
    assert(decoded.name == "Ada")
    val encoded: Xml.Element = codec.encode(decoded)
    assert(encoded.get("xml:id").contains("x"))
    assert(encoded.attributes.find(_._1.qualifiedName == "xml:id").get._1.namespace.contains(XmlNamespace.xml.uri))
  }

  test("leftover xmlns is not an error") {
    val codec: XmlCodec[Box] = XmlCodec.derived(using Box.schema)
    val decoded: Box = codec.decode(parse("""<Box xmlns:ex="http://example.com/ns" n="1"/>""")).toOption.get
    assert(decoded.n == "1")
  }

  test("namespace modifiers encode xmlns and a prefixed name") {
    val codec: XmlCodec[NsBox] = XmlCodec.derived(using NsBox.schema)
    val encoded: Xml.Element = codec.encode(NsBox("1"))
    assert(encoded.getName == "ex:NsBox")
    assert(encoded.name.localName == "NsBox")
    assert(encoded.name.prefix.contains("ex"))
    assert(encoded.name.namespace.contains("http://example.com/ns"))
    assert(encoded.get("xmlns:ex").contains("http://example.com/ns"))
    assert(encoded.get("n").contains("1"))
    val decoded: NsBox = codec.decode(parse("""<ex:NsBox xmlns:ex="http://example.com/ns" n="1"/>""")).toOption.get
    assert(decoded.n == "1")
    val scalaEl: ScalaXml.Element = codec.encode(NsBox("1"))
    assert(ScalaXml.getName(scalaEl) == "ex:NsBox")
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

  test("parseCatalog loads a classpath catalog") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val decoded: Seq[Language] = XmlParser.parseCatalog(
      classOf[XmlCodecSpec],
      "languages.xml",
      "Languages",
      codec
    ).toOption.get
    assert(decoded.map(_.ident) == Seq("ru", "he"))
  }

  test("nested recursive children round-trip") {
    val codec: XmlCodec[Node] = XmlCodec.derived(using Node.schema)
    val xml: String = """<node n="root"><node n="a"/><node n="b"><node n="c"/></node></node>"""
    val decoded: Node = codec.decode(parse(xml)).toOption.get
    assert(decoded == Node("root", Seq(Node("a"), Node("b", Seq(Node("c"))))))
    val encoded: Xml.Element = codec.encode(decoded)
    assert(codec.decode(encoded).toOption.get == decoded)
  }

final case class Language(
  @Modifier.config(XmlCodec.Attribute, "") ident: String
) derives CanEqual
object Language:
  given schema: Schema[Language] = Schema.derived

final case class LangUsage(
  @Modifier.config(XmlCodec.Element, "language") languages: Seq[Language]
) derives CanEqual
object LangUsage:
  given schema: Schema[LangUsage] = Schema.derived

final case class EntityName(
  @Modifier.config(XmlCodec.Attribute, "") id: Option[String] = None,
  @Modifier.config(XmlCodec.Attribute, "") ref: Option[String] = None,
  @Modifier.config(XmlCodec.Text, "") name: String
) derives CanEqual
object EntityName:
  given schema: Schema[EntityName] = Schema.derived

final case class Flag(
  @Modifier.config(XmlCodec.Attribute, "") on: Boolean
) derives CanEqual
object Flag:
  given schema: Schema[Flag] = Schema.derived

final case class Box(
  @Modifier.config(XmlCodec.Attribute, "") n: String
) derives CanEqual
object Box:
  given schema: Schema[Box] = Schema.derived

final case class Text(
  @Modifier.config(XmlCodec.Attribute, "") lang: Option[String],
  @Modifier.config(XmlCodec.Element, "body") body: Xml.Element
) derives CanEqual
object Text:
  import XmlCodec.xmlElementSchema
  given schema: Schema[Text] = Schema.derived

sealed trait Part derives CanEqual
@Modifier.config(XmlCodec.Element, "positive")
final case class Positive(@Modifier.config(XmlCodec.Attribute, "") n: Int) extends Part derives CanEqual
@Modifier.config(XmlCodec.Element, "negative")
final case class Negative(@Modifier.config(XmlCodec.Attribute, "") n: Int) extends Part derives CanEqual
object Part:
  given schema: Schema[Part] = Schema.derived

final case class Lesson(parts: Seq[Part]) derives CanEqual
object Lesson:
  given schema: Schema[Lesson] = Schema.derived

final case class Named(
  @Modifier.config(XmlCodec.Attribute, "xml:id") id: Option[String],
  @Modifier.config(XmlCodec.Text, "") name: String
) derives CanEqual
object Named:
  given schema: Schema[Named] = Schema.derived

@Modifier.config(XmlCodec.NamespaceUri, "http://example.com/ns")
@Modifier.config(XmlCodec.NamespacePrefix, "ex")
final case class NsBox(
  @Modifier.config(XmlCodec.Attribute, "") n: String
) derives CanEqual
object NsBox:
  given schema: Schema[NsBox] = Schema.derived

final case class Title(@Modifier.config(XmlCodec.Text, "") value: String) derives CanEqual
object Title:
  given schema: Schema[Title] = Schema.derived

final case class Book(title: Option[Title]) derives CanEqual
object Book:
  given schema: Schema[Book] = Schema.derived

@Modifier.config(XmlCodec.Element, "node")
final case class Node(
  @Modifier.config(XmlCodec.Attribute, "") n: String,
  @Modifier.config(XmlCodec.Element, "node") children: Seq[Node] = Seq.empty
) derives CanEqual
object Node:
  given schema: Schema[Node] = Schema.derived

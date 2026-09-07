package org.podval.xml

import zio.blocks.schema.xml.Xml
import org.xml.sax.InputSource
import scala.util.Using
import java.io.File
import java.net.URL

/** Load XML (and HTML) into any [[XmlAst]].
  *
  * `xi:include/@href` stays in the tree. Publisher `store`/`collection`
  * indexes treat it as a child page, not an inlined document. HTML uses
  * TagSoup (`parseHtml`) from a string, URL, or file.
  *
  * `parseXml` / `parseResource` return the document element. Prolog/epilog
  * comments, PIs, and the doctype are on [[XmlDocument]] from
  * `parseXmlDocument` / `parseResourceDocument`.
  *
  * `E` is inferred from the expected type or a unique `XmlAst` given. Catalog
  * helpers pin ZIO Blocks XML internally.
  */
object XmlParser:
  def parse[E: XmlAst](content: String, isXml: Boolean): Either[Throwable, E] =
    if isXml then parseXml(content) else parseHtml(content)

  /** SAX, not StAX: JDK SAX preserves CDATA via `LexicalHandler`. */
  def parseXml[E: XmlAst](content: String): Either[Throwable, E] =
    parseXmlDocument(content).map(_.root)

  def parseXml[E: XmlAst](file: File): Either[Throwable, E] =
    parseXml(file.toURI.toURL)

  def parseXml[E: XmlAst](url: URL): Either[Throwable, E] =
    parseXmlDocument(url).map(_.root)

  def parseXmlDocument[E: XmlAst](content: String): Either[Throwable, XmlDocument[E]] =
    XmlParserSax.parseXmlDocument(content)

  def parseXmlDocument[E: XmlAst](file: File): Either[Throwable, XmlDocument[E]] =
    parseXmlDocument(file.toURI.toURL)

  def parseXmlDocument[E: XmlAst](url: URL): Either[Throwable, XmlDocument[E]] =
    Using(url.openStream()): stream =>
      val source: InputSource = InputSource(stream)
      source.setSystemId(url.toString)
      XmlParserSax.parseXmlDocument(source)
    .fold(Left(_), identity)

  /** Classpath resource; `name` is `Class.getResource` style (`/org/.../Foo.xml`
    * is from the classpath root). */
  def parseResource[E: XmlAst](name: String): Either[Throwable, E] =
    val absolute: String = if name.startsWith("/") then name else s"/$name"
    parseResource(XmlParser.getClass, absolute)

  def parseResource[E: XmlAst](loader: Class[?], name: String): Either[Throwable, E] =
    parseResourceDocument(loader, name).map(_.root)

  def parseResourceDocument[E: XmlAst](name: String): Either[Throwable, XmlDocument[E]] =
    val absolute: String = if name.startsWith("/") then name else s"/$name"
    parseResourceDocument(XmlParser.getClass, absolute)

  def parseResourceDocument[E: XmlAst](loader: Class[?], name: String): Either[Throwable, XmlDocument[E]] =
    Option(loader.getResource(name)) match
      case None => Left(XmlError(s"Resource not found: $name"))
      case Some(url) => parseXmlDocument(url)

  /** Class simple name without a trailing `$` (`Selector$` → `Selector`). */
  def className(loader: Class[?]): String = loader.getSimpleName.replace("$", "")

  /** Parse a catalog resource: wrapper `name`, each child decoded with `codec`. */
  // Returns `Seq[A]`, so `E` cannot be inferred. Pin ZIO Blocks XML (`Xml.Element`):
  // in this package `Xml`, `Html`, and `ScalaXml` would otherwise be ambiguous.
  // `loadCatalog` unwraps these methods; call sites never choose an AST.
  def parseCatalog[A](resource: String, name: String, codec: XmlCodec[A]): Either[Throwable, Seq[A]] =
    parseResource[Xml.Element](resource).flatMap: root =>
      codec.decodeCatalog(root, name).left.map(e => e: Throwable)

  def parseCatalog[A](loader: Class[?], codec: XmlCodec[A]): Either[Throwable, Seq[A]] =
    val name: String = className(loader)
    parseCatalog(loader, s"$name.xml", name, codec)

  def parseCatalog[A](loader: Class[?], name: String, codec: XmlCodec[A]): Either[Throwable, Seq[A]] =
    parseCatalog(loader, s"$name.xml", name, codec)

  def parseCatalog[A](
    loader: Class[?],
    resource: String,
    name: String,
    codec: XmlCodec[A]
  ): Either[Throwable, Seq[A]] =
    parseResource[Xml.Element](loader, resource).flatMap: root =>
      codec.decodeCatalog(root, name).left.map(e => e: Throwable)

  /** Like [[parseCatalog]] but throws. Catalog file and wrapper name come from
    * `from.getClass` (`Foo` → `Foo.xml` / `<Foo>`). */
  def loadCatalog[A](from: AnyRef, codec: XmlCodec[A]): Seq[A] =
    unwrap(parseCatalog(from.getClass, codec))

  def loadCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A]): Seq[A] =
    unwrap(parseCatalog(from.getClass, name, codec))

  private def unwrap[A](result: Either[Throwable, Seq[A]]): Seq[A] =
    result.fold(error => throw error, identity)

  def parseHtml[E: XmlAst](content: String): Either[Throwable, E] =
    XmlParserSax.parse(reader = HtmlTagSoup.reader, content = content)

  def parseHtml[E: XmlAst](file: File): Either[Throwable, E] =
    parseHtml(file.toURI.toURL)

  def parseHtml[E: XmlAst](url: URL): Either[Throwable, E] =
    Using(url.openStream()): stream =>
      val source: InputSource = InputSource(stream)
      source.setSystemId(url.toString)
      XmlParserSax.parse(reader = HtmlTagSoup.reader, source = source)
    .fold(Left(_), identity)

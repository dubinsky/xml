package org.podval.xml

import zio.blocks.schema.xml.Xml
import org.xml.sax.InputSource
import scala.util.Using
import java.net.URL

/** Load XML (and HTML) into any [[XmlAst]].
  *
  * `xi:include/@href` stays in the tree. Publisher `store`/`collection`
  * indexes treat it as a child page, not an inlined document. HTML uses
  * TagSoup (`parseHtml`).
  *
  * `parseXml` / `parseResource` return the document element. Prolog/epilogue
  * comments, PIs, and the doctype are on [[XmlDocument]] from
  * `parseXmlDocument`.
  *
  * `E` is inferred from the expected type or `given Xml`. HTML and Scala XML
  * need `import Html.given` / `import ScalaXml.given`.
  * Catalog helpers pin ZIO Blocks XML internally.
  */
object XmlParser:
  def parse[E: XmlAst](content: String, isXml: Boolean): Either[Throwable, E] =
    if isXml then parseXml(content) else parseHtml(content)

  /** SAX, not StAX: JDK SAX preserves CDATA via `LexicalHandler`. */
  def parseXml[E: XmlAst](content: String): Either[Throwable, E] =
    parseXmlDocument(content).map(_.root)

  def parseXmlDocument[E: XmlAst](content: String): Either[Throwable, XmlDocument[E]] =
    XmlParserSax.parseXmlDocument(content)

  /** Classpath resource next to `loader` (`Class.getResource`). */
  def parseResource[E: XmlAst](loader: Class[?], name: String): Either[Throwable, E] =
    Option(loader.getResource(name)) match
      case None => Left(XmlError(s"Resource not found: $name"))
      case Some(url) => parseXmlDocument(url).map(_.root)

  /** Class simple name without a trailing `$` (`Selector$` → `Selector`). */
  def className(loader: Class[?]): String = loader.getSimpleName.replace("$", "")

  /** Catalog of `codec` children. File and wrapper name come from
    * `from.getClass` (`Foo` → `Foo.xml` / `<Foo>`). Throws on error. */
  def loadCatalog[A](from: AnyRef, codec: XmlCodec[A]): Seq[A] =
    loadCatalog(from, className(from.getClass), codec)

  def loadCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A]): Seq[A] =
    unwrap:
      parseResource[Xml.Element](from.getClass, s"$name.xml").flatMap: root =>
        codec.decodeCatalog(root, name).left.map(e => e: Throwable)

  private def unwrap[A](result: Either[Throwable, Seq[A]]): Seq[A] =
    result.fold(error => throw error, identity)

  def parseHtml[E: XmlAst](content: String): Either[Throwable, E] =
    XmlParserSax.parse(reader = HtmlTagSoup.reader, content = content).map(_.result)

  private def parseXmlDocument[E: XmlAst](url: URL): Either[Throwable, XmlDocument[E]] =
    Using(url.openStream()): stream =>
      val source: InputSource = InputSource(stream)
      source.setSystemId(url.toString)
      XmlParserSax.parseXmlDocument(source)
    .fold(Left(_), identity)

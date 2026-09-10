package org.podval.xml

import zio.blocks.schema.xml.Xml
import org.xml.sax.InputSource
import scala.util.Using
import java.io.StringReader

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

  def parseHtml[E: XmlAst](content: String): Either[Throwable, E] =
    XmlParserSax.parseDocument(reader = HtmlTagSoup.reader, toInputSource(content)).map(_.root)

  /** SAX, not StAX: JDK SAX preserves CDATA via `LexicalHandler`. */
  def parseXml[E: XmlAst](content: String): Either[Throwable, E] =
    parseXmlDocument(content).map(_.root)

  def parseXmlDocument[E: XmlAst](content: String): Either[Throwable, XmlDocument[E]] =
    parseXmlDocument(toInputSource(content))

  /** Classpath resource next to `loader` (`Class.getResource`). */
  def parseResource[E: XmlAst](loader: Class[?], name: String): Either[Throwable, E] =
    Option(loader.getResource(name)) match
      case None => Left(XmlError(s"Resource not found: $name"))
      case Some(url) =>
        Using(url.openStream()): stream =>
          val source: InputSource = InputSource(stream)
          source.setSystemId(url.toString)
          parseXmlDocument(source).map(_.root)
        .fold(Left(_), identity)

  private def parseXmlDocument[E: XmlAst](source: InputSource): Either[Throwable, XmlDocument[E]] = XmlParserSax
    .parseDocument(XmlParserSax.xmlReader, source)
    .map(_.copy(declaration = Some(XmlDeclaration())))

  private def toInputSource(content: String): InputSource = InputSource(StringReader(content))

  /** Class simple name without a trailing `$` (`Selectors$` → `Selectors`). */
  def className(loader: Class[?]): String = loader.getSimpleName.replace("$", "")

  /** Catalog of `codec` children. File and wrapper name come from
    * `from.getClass` (`object Foo` → `Foo.xml` / `<Foo>` next to that class).
    * Throws on a missing resource or decode error.
    * When the object is not named after the file, pass the name:
    * `loadCatalog(from, "Selector", codec)` (`object Selectors` → `Selector.xml`). */
  def loadCatalog[A](from: AnyRef, codec: XmlCodec[A]): Seq[A] =
    loadCatalog(from, className(from.getClass), codec)

  /** Like `loadCatalog(from, codec)` with an explicit resource and wrapper `name`. */
  def loadCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A]): Seq[A] =
      parseResource[Xml.Element](from.getClass, s"$name.xml")
        .flatMap(root => codec.decodeCatalog(root, name).left.map(e => e: Throwable))
        .fold(error => throw error, identity)

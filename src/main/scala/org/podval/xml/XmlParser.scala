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
// TODO clean up
object XmlParser:
  def parse[E: XmlAst](content: String, isXml: Boolean): Either[Throwable, E] =
    if isXml then parseXml(content) else parseHtml(content)

  /** SAX, not StAX: JDK SAX preserves CDATA via `LexicalHandler`. */
  def parseXml[E: XmlAst](content: String): Either[Throwable, E] =
    parseXmlDocument(content).map(_.root)

  def parseXmlDocument[E: XmlAst](content: String): Either[Throwable, XmlDocument[E]] =
    parseXmlDocumentAndOverrideDeclaration(toInputSource(content))

  def parseHtml[E: XmlAst](content: String): Either[Throwable, E] =
    XmlParserSax.parseDocument(reader = HtmlTagSoup.reader, toInputSource(content)).map(_.root)

  private def parseXmlDocumentAndOverrideDeclaration[E: XmlAst](source: InputSource): Either[Throwable, XmlDocument[E]] =
    XmlParserSax.parseDocument(XmlParserSax.xmlReader, source).map: document =>
      document.copy(declaration = Some(XmlDeclaration())) // TODO why override declaration?

  /** Classpath resource next to `loader` (`Class.getResource`). */
  def parseResource[E: XmlAst](loader: Class[?], name: String): Either[Throwable, E] =
    Option(loader.getResource(name)) match
      case None => Left(XmlError(s"Resource not found: $name"))
      case Some(url) =>
        Using(url.openStream()): stream =>
          val source: InputSource = InputSource(stream)
          source.setSystemId(url.toString)
          parseXmlDocumentAndOverrideDeclaration(source).map(_.root)
        .fold(Left(_), identity)

  private def toInputSource(content: String): InputSource = InputSource(StringReader(content))

  /** Class simple name without a trailing `$` (`Selector$` → `Selector`). */
  def className(loader: Class[?]): String = loader.getSimpleName.replace("$", "")

  /** Catalog of `codec` children. File and wrapper name come from
    * `from.getClass` (`Foo` → `Foo.xml` / `<Foo>`). Throws on error. */
  def loadCatalog[A](from: AnyRef, codec: XmlCodec[A]): Seq[A] =
    loadCatalog(from, className(from.getClass), codec)

  def loadCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A]): Seq[A] =
      parseResource[Xml.Element](from.getClass, s"$name.xml")
        .flatMap(root => codec.decodeCatalog(root, name).left.map(e => e: Throwable))
        .fold(error => throw error, identity)

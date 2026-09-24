package org.podval.xml

import org.xml.sax.InputSource
import scala.util.Using
import java.io.StringReader

/** Load XML and HTML into [[Xml]].
  *
  * `xi:include/@href` stays in the tree. Publisher `store`/`collection`
  * indexes treat it as a child page, not an inlined document. HTML uses
  * TagSoup (`parseHtml`).
  *
  * `parseXml` / `parseResource` return the document element. Prolog/epilogue
  * comments, PIs, and the doctype are on [[XmlDocument]] from
  * `parseXmlDocument`.
  * Another tree is `element.to[TO]` after parse.
  *
  * I/O returns `Either[XmlError, _]`. `loadCatalog` / `loadResources` throw
  * (programmer catalogs, like `Stores.resolve`); use `attemptCatalog` /
  * `attemptResources` to stay in `Either`.
  */
object XmlParser:
  def parse(content: String, isXml: Boolean): Either[XmlError, Xml.Element] =
    if isXml then parseXml(content) else parseHtml(content)

  def parseHtml(content: String): Either[XmlError, Xml.Element] =
    XmlParserSax.parseDocument(reader = HtmlTagSoup.reader, toInputSource(content)).map(_.root).left.map(asXmlError)

  /** SAX, not StAX: JDK SAX preserves CDATA via `LexicalHandler`. */
  def parseXml(content: String): Either[XmlError, Xml.Element] =
    parseXmlDocument(toInputSource(content)).map(_.root)

  def parseXmlDocument(content: String): Either[XmlError, XmlDocument[Xml.Element]] =
    parseXmlDocument(toInputSource(content))

  private def parseXmlDocument(source: InputSource): Either[XmlError, XmlDocument[Xml.Element]] = XmlParserSax
    .parseDocument(XmlParserSax.xmlReader, source)
    .left.map(asXmlError)
    .map(_.copy(declaration = Some(XmlDeclaration())))

  /** Classpath resource next to `loader` (`Class.getResource`). */
  def parseResource(loader: Class[?], name: String): Either[XmlError, Xml.Element] =
    Option(loader.getResource(name)) match
      case None => Left(XmlError(s"Resource not found: $name"))
      case Some(url) =>
        Using(url.openStream()): stream =>
          val source: InputSource = InputSource(stream)
          source.setSystemId(url.toString)
          parseXmlDocument(source).map(_.root)
        .fold(e => Left(asXmlError(e)), identity)

  private def asXmlError(error: Throwable): XmlError = error match
    case e: XmlError => e
    case e => XmlError(Option(e.getMessage).getOrElse(e.toString), e)

  private def toInputSource(content: String): InputSource = InputSource(StringReader(content))

  /** Class simple name without a trailing `$` (`Selectors$` → `Selectors`). */
  def className(loader: Class[?]): String = loader.getSimpleName.replace("$", "")

  /** Catalog of `codec` children. File and wrapper name come from
    * `from.getClass` (`object Foo` → `Foo.xml` / `<Foo>` next to that class).
    * Throws on a missing resource or decode error (programmer catalog).
    * I/O without throwing is [[attemptCatalog]].
    * When the Scala object is not named after the file, pass the name:
    * `loadCatalog(from, "Selector", codec)` (`object Selectors` → `Selector.xml`). */
  def loadCatalog[A](from: AnyRef, codec: XmlCodec[A]): Seq[A] =
    loadCatalog(from, className(from.getClass), codec)

  /** Like `loadCatalog(from, codec)` with an explicit resource and wrapper `name`. */
  def loadCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A]): Seq[A] =
    loadCatalog(from, name, codec, name)

  /** Resource `name.xml` next to `from`; catalog wrapper `wrapperName`. */
  def loadCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A], wrapperName: String): Seq[A] =
    attemptCatalog(from, name, codec, wrapperName).fold(error => throw error, identity)

  def attemptCatalog[A](from: AnyRef, codec: XmlCodec[A]): Either[XmlError, Seq[A]] =
    attemptCatalog(from, className(from.getClass), codec)

  def attemptCatalog[A](from: AnyRef, name: String, codec: XmlCodec[A]): Either[XmlError, Seq[A]] =
    attemptCatalog(from, name, codec, name)

  def attemptCatalog[A](
    from: AnyRef,
    name: String,
    codec: XmlCodec[A],
    wrapperName: String
  ): Either[XmlError, Seq[A]] =
    parseResource(from.getClass, s"$name.xml")
      .flatMap(root => codec.decodeCatalog(root, wrapperName))

  /** Each `name.xml` next to `from` decoded as one document (the root element). Throws. */
  def loadResources[A](from: AnyRef, codec: XmlCodec[A], names: String*): Seq[A] =
    attemptResources(from, codec, names*).fold(error => throw error, identity)

  def attemptResources[A](from: AnyRef, codec: XmlCodec[A], names: String*): Either[XmlError, Seq[A]] =
    names.foldLeft(Right(Vector.empty[A]): Either[XmlError, Vector[A]]): (acc, name) =>
      for
        items <- acc
        item <- parseResource(from.getClass, s"$name.xml").flatMap(codec.decode)
      yield items :+ item
    .map(_.toSeq)

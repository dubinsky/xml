package org.podval.xml

import org.xml.sax.{Attributes, InputSource, XMLReader}
import org.xml.sax.ext.LexicalHandler
import org.xml.sax.helpers.DefaultHandler
import javax.xml.parsers.SAXParserFactory

// Note: written by Grok, re-written by me ;)
private[xml] object XmlParserSax:
  /** JDK SAX. `LexicalHandler` reports CDATA as a distinct node kind. */
  def xmlReader: XMLReader =
    val factory: SAXParserFactory = SAXParserFactory.newInstance
    factory.setNamespaceAware(true)
    factory.setValidating(false)
    factory.setXIncludeAware(false)
    val reader: XMLReader = factory.newSAXParser.getXMLReader
    // So undeclared `&nbsp;` (FlexMark HTML-as-XML) can become skippedEntity, not a halt.
    try reader.setFeature("http://apache.org/xml/features/continue-after-fatal-error", true)
    catch case _: Exception => ()
    try reader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    catch case _: Exception => ()
    reader
  
  def parseDocument(
    reader: XMLReader,
    source: InputSource
  ): Either[Throwable, XmlDocument[Xml.Element]] =
    try
      reader.setFeature("http://xml.org/sax/features/namespaces", true)
      // Include xmlns:* in the attribute list so namespace declarations become attributes
      reader.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
      // Put xmlns:* in the xmlns namespace (off by default for SAX1 compatibility).
      reader.setFeature("http://xml.org/sax/features/xmlns-uris", true)
      reader.setFeature("http://xml.org/sax/features/external-general-entities", false)
      reader.setFeature("http://xml.org/sax/features/external-parameter-entities", false)

      val builder: XmlBuilder = XmlBuilder()
      val handler: XmlParserSax = XmlParserSax(builder)

      reader.setContentHandler(handler)
      reader.setErrorHandler(handler)
      reader.setProperty("http://xml.org/sax/properties/lexical-handler", handler)
      reader.parse(source)

      Right(builder.document)
    catch
      case e: Throwable => Left(e)

private final class XmlParserSax(
  builder: XmlBuilder
) extends DefaultHandler with LexicalHandler:
  private var inCData: Boolean = false
  private var inDtd: Boolean = false
  private val cdata: StringBuilder = StringBuilder()

  private def charactersToBuilder(characters: Array[Char], start: Int, length: Int): Unit =
    val text: String = String(characters, start, length)
    if inCData then cdata.addAll(text) else builder.text(text)

  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    attributes: Attributes
  ): Unit = builder.startElement(
    fromName(uri, localName, qName, isAttribute = false),
    fromAttributes(attributes)
  )

  override def endElement(uri: String, localName: String, qName: String): Unit =
    builder.endElement()

  override def characters(characters: Array[Char], start: Int, length: Int): Unit =
    charactersToBuilder(characters, start, length)

  override def ignorableWhitespace(characters: Array[Char], start: Int, length: Int): Unit =
    charactersToBuilder(characters, start, length)

  override def processingInstruction(target: String, data: String): Unit =
    if !inDtd then builder.processingInstruction(target = target, data = data)

  // LexicalHandler

  override def startDTD(name: String, publicId: String, systemId: String): Unit =
    inDtd = true
    builder.doctype(
      name,
      Option(publicId).filter(_.nonEmpty),
      Option(systemId).filter(_.nonEmpty)
    )

  override def endDTD(): Unit =
    inDtd = false

  override def startEntity(name: String): Unit = ()

  override def endEntity(name: String): Unit = ()

  override def skippedEntity(name: String): Unit =
    // Undeclared `&nbsp;` stays as the characters `&nbsp;` (the writer does not
    // encode that `&`).
    builder.text(s"&$name;")

  // Undeclared entities (`&nbsp;` in Markdown HTML-as-XML): skip the SAX fatal
  // so skippedEntity can emit `&name;`.
  override def fatalError(e: org.xml.sax.SAXParseException): Unit =
    if Option(e.getMessage).exists(_.contains("was referenced, but not declared")) then ()
    else throw e

  override def startCDATA(): Unit =
    inCData = true

  override def endCDATA(): Unit =
    inCData = false
    builder.cdata(cdata.toString)
    cdata.clear()

  override def comment(characters: Array[Char], start: Int, length: Int): Unit =
    if !inDtd then builder.comment(String(characters, start, length))

private def fromName(
  uri: String,
  localName: String,
  qName: String,
  isAttribute: Boolean
): XmlName =
  val (prefix: Option[String], local: String) =
    val (pre, rest) = qName.span(_ != ':')
    if localName.nonEmpty then (Option.when(rest.nonEmpty)(pre).filter(_.nonEmpty), localName)
    else if rest.nonEmpty then (Some(pre), rest.drop(1))
    else (None, qName)

  XmlName(
    localName = local,
    namespace = XmlNamespace.of(
      prefix,
      XmlNamespace
        .wellKnown(prefix, local, isAttribute)
        .map(_.uri)
        .orElse(Option.when(uri.nonEmpty)(uri))
    )
  )

private def fromAttributes(
  attributes: Attributes
): Seq[(XmlName, String)] = (0 until attributes.getLength).map: i =>
  (
    fromName(
      uri = attributes.getURI(i),
      localName = attributes.getLocalName(i),
      qName = attributes.getQName(i),
      isAttribute = true
    ),
    attributes.getValue(i)
  )

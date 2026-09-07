package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import org.xml.sax.{Attributes, InputSource, XMLReader}
import org.xml.sax.ext.LexicalHandler
import org.xml.sax.helpers.DefaultHandler
import javax.xml.parsers.SAXParserFactory
import java.io.{InputStream, Reader, StringReader}

// Note: written by Grok, re-written by me ;)
object XmlParserSax:
  /** JDK SAX. `LexicalHandler` reports CDATA; JDK StAX reports it as CHARACTERS
    * (`isCData=false`), so XML is parsed here, not with StAX. */
  def xmlReader: XMLReader =
    val factory: SAXParserFactory = SAXParserFactory.newInstance
    factory.setNamespaceAware(true)
    factory.setValidating(false)
    factory.setXIncludeAware(false)
    val reader: XMLReader = factory.newSAXParser.getXMLReader
    // So undeclared `&nbsp;` (FlexMark HTML-as-XML) can become skippedEntity, not a halt.
    try reader.setFeature("http://apache.org/xml/features/continue-after-fatal-error", true)
    catch case _: Exception => ()
    reader

  def parseXml(content: String): Either[Throwable, Xml.Element] =
    parse(xmlReader, content)

  def parseXml(stream: InputStream): Either[Throwable, Xml.Element] =
    parse(xmlReader, stream)

  def parseXml(source: InputSource): Either[Throwable, Xml.Element] =
    parse(xmlReader, source)

  def parse(reader: XMLReader, content: String): Either[Throwable, Xml.Element] =
    parse(reader, InputSource(StringReader(content)))

  def parse(reader: XMLReader, stream: InputStream): Either[Throwable, Xml.Element] =
    parse(reader, InputSource(stream))

  def parse(reader: XMLReader, characterReader: Reader): Either[Throwable, Xml.Element] =
    parse(reader, InputSource(characterReader))

  def parse(reader: XMLReader, source: InputSource): Either[Throwable, Xml.Element] =
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

      Right(builder.result)
    catch
      case e: Throwable => Left(e)  

private final class XmlParserSax(builder: XmlBuilder) extends DefaultHandler with LexicalHandler:
  private var inCData: Boolean = false
  private val cdata: StringBuilder = StringBuilder()

  private def charactersToBuilder(characters: Array[Char], start: Int, length: Int): Unit =
    val text: String = String(characters, start, length)
    if inCData then cdata.addAll(text) else builder.text(text)

  override def startElement(
    uri: String,
    localName: String,
    qName: String,
    attributes: Attributes
  ): Unit =
    builder.startElement(Xml.Element(
      name = fromName(uri, localName, qName, isAttribute = false),
      children = Chunk.empty,
      attributes = fromAttributes(attributes)
    ))

  override def endElement(uri: String, localName: String, qName: String): Unit =
    builder.endElement()

  override def characters(characters: Array[Char], start: Int, length: Int): Unit =
    charactersToBuilder(characters, start, length)

  override def ignorableWhitespace(characters: Array[Char], start: Int, length: Int): Unit =
    charactersToBuilder(characters, start, length)

  override def processingInstruction(target: String, data: String): Unit =
    builder.processingInstruction(target = target, data = data)

  // LexicalHandler

  override def startDTD(name: String, publicId: String, systemId: String): Unit = ()

  override def endDTD(): Unit = ()

  override def startEntity(name: String): Unit = ()

  override def endEntity(name: String): Unit = ()

  override def skippedEntity(name: String): Unit =
    builder.text(s"&$name;")

  // Undeclared entities (`&nbsp;` in Markdown HTML-as-XML): JDK StAX reported
  // EntityReference; SAX fatals. Skip that fatal so skippedEntity can emit `&name;`.
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
    builder.comment(String(characters, start, length))

private def fromName(uri: String, localName: String, qName: String, isAttribute: Boolean): XmlName =
  val (prefix: Option[String], local: String) =
    if localName.nonEmpty then
      val colon: Int = qName.indexOf(':')
      val p: Option[String] = Option.when(colon >= 0)(qName.substring(0, colon)).filter(_.nonEmpty)
      (p, localName)
    else
      val colon: Int = qName.indexOf(':')
      if colon >= 0 then (Some(qName.substring(0, colon)), qName.substring(colon + 1))
      else (None, qName)

  XmlName(
    localName = local,
    prefix = prefix,
    namespace = namespaceOf(uri, prefix, local, isAttribute)
  )

private def namespaceOf(
  uri: String,
  prefix: Option[String],
  local: String,
  isAttribute: Boolean
): Option[String] =
  XmlNamespace.wellKnown(prefix, local, isAttribute)
    // TagSoup puts the XHTML namespace on every HTML element; drop it.
    // TODIO is this the best place to drop it?
    .orElse(noneIfEmpty(uri).filterNot(_ == XmlNamespace.xhtml))

private def fromAttributes(attributes: Attributes): Chunk[(XmlName, String)] =
  Chunk.from((0 until attributes.getLength).map: i =>
    (
      fromName(
        uri = attributes.getURI(i),
        localName = attributes.getLocalName(i),
        qName = attributes.getQName(i),
        isAttribute = true
      ),
      attributes.getValue(i)
    )
  )

private def noneIfEmpty(string: String): Option[String] =
  Option.when(string.nonEmpty)(string)


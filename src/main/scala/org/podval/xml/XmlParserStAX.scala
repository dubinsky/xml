package org.podval.xml

import scala.jdk.CollectionConverters.IteratorHasAsScala
import java.io.{InputStream, Reader, StringReader}
import javax.xml.namespace.QName
import javax.xml.stream.{XMLEventReader, XMLInputFactory, XMLStreamException}
import javax.xml.stream.events.{Attribute, Characters, Comment, EndElement, EntityReference, ProcessingInstruction,
  StartElement}

// Note: not used; kept for fun.
object XmlParserStAX:
  def parse[E: XmlAst](content: String): Either[XMLStreamException, E] =
    parse(StringReader(content))

  def parse[E: XmlAst](reader: Reader): Either[XMLStreamException, E] =
    try parseEventReader(factory.createXMLEventReader(reader))
    catch case e: XMLStreamException => Left(e)

  def parse[E: XmlAst](stream: InputStream): Either[XMLStreamException, E] =
    try parseEventReader(factory.createXMLEventReader(stream))
    catch case e: XMLStreamException => Left(e)

  private def factory: XMLInputFactory =
    // Built-in: com.sun.xml.internal.stream.XMLInputFactoryImp
    val result: XMLInputFactory = XMLInputFactory.newInstance
    // Store `xi:include` is a page reference. Do not expand XInclude or load a DTD/external
    // subset (there is no standard StAX XInclude switch; the JDK factory leaves it off).
    result.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
    result.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    result.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    result

  private def parseEventReader[E: XmlAst](reader: XMLEventReader): Either[XMLStreamException, E] =
    try Right(read(reader))
    catch case e: XMLStreamException => Left(e)
    finally reader.close()

  private def read[E: XmlAst](reader: XMLEventReader): E =
    val builder: XmlBuilder[E] = XmlBuilder()

    while reader.hasNext do reader.nextEvent match
      case startElement: StartElement =>
        builder.startElement(
          fromQName(startElement.getName),
          startElement.getAttributes.asScala.map(fromAttribute).toSeq ++
            startElement.getNamespaces.asScala.map(fromAttribute).toSeq
        )

      case endElement: EndElement =>
        builder.endElement()

      case characters: Characters =>
        val text: String = characters.getData
        // JDK StAX reports CDATA as CHARACTERS with isCData=false; Woodstox sets the flag.
        if characters.isCData then builder.cdata(text) else builder.text(text)

      case entityReference: EntityReference =>
        builder.text(s"&${entityReference.getName};")

      case comment: Comment =>
        builder.comment(comment.getText)

      case processingInstruction: ProcessingInstruction =>
        builder.processingInstruction(
          target = processingInstruction.getTarget,
          data = processingInstruction.getData
        )

      case _ => ()

    builder.result

  private def fromQName(qName: QName, isAttribute: Boolean): XmlExpandedName =
    val prefix: Option[String] = noneIfEmpty(qName.getPrefix)
    val local: String = qName.getLocalPart
    XmlExpandedName(
      localName = local,
      prefix = prefix,
      namespace = namespaceOf(qName.getNamespaceURI, prefix, local, isAttribute)
    )

  private def namespaceOf(
    uri: String,
    prefix: Option[String],
    local: String,
    isAttribute: Boolean
  ): Option[String] =
    XmlNamespace.wellKnown(prefix, local, isAttribute).orElse(noneIfEmpty(uri))

  private def fromQName(qName: QName): XmlExpandedName = fromQName(qName, isAttribute = false)

  private def noneIfEmpty(string: String): Option[String] =
    Option.when(string.nonEmpty)(string)

  // Note: this takes care of the namespaces too - `Namespace` is derived from `Attribute`,
  // and `NamespaceImpl` handles the `xmlns:` prefix.
  private def fromAttribute(attribute: Attribute): (XmlExpandedName, String) = (
    fromQName(attribute.getName, isAttribute = true),
    attribute.getValue
  )

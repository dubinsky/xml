package org.podval.xml

import org.ccil.cowan.tagsoup.Parser as TagSoupParser
import org.xml.sax.{Attributes, XMLFilter, XMLReader}
import org.xml.sax.helpers.{AttributesImpl, XMLFilterImpl}
import scala.collection.mutable

// Note: TagSoup is not suitable for case-sensitive or namespaced XML dialects such as TEI, since it:
// - does not support namespaces at all;
// - lower-cases element and attribute names.
object HtmlTagSoup:
  def reader: XMLReader =
    val tagSoup: XMLReader = TagSoupParser()
    tagSoup.setFeature(TagSoupParser.rootBogonsFeature, true)
    // Do not invent HTML default attributes (e.g. <br clear="none">).
    tagSoup.setFeature(TagSoupParser.defaultAttributesFeature, false)

    val filter: XMLFilter = TagSoupFilter()
    filter.setParent(tagSoup)
    filter

  private val wrappers: Set[String] = Set(XmlElement.Html, XmlElement.Body).map(_.localName)

  // TagSoup wraps fragments in <html><body>...</body></html> and puts the XHTML
  // namespace on every element. This filter undoes both.
  private final class TagSoupFilter extends XMLFilterImpl:
    private val suppressedPrefixes: mutable.Set[String] = mutable.Set.empty

    private def suppress(localName: String): Boolean = HtmlTagSoup.wrappers.contains(localName)

    private def dropXhtml(uri: String): String =
      if uri == XmlNamespace.xhtml.uri then "" else uri

    override def startPrefixMapping(prefix: String, uri: String): Unit =
      if uri == XmlNamespace.xhtml.uri then suppressedPrefixes += prefix
      else super.startPrefixMapping(prefix, uri)

    override def endPrefixMapping(prefix: String): Unit =
      if !suppressedPrefixes.remove(prefix) then super.endPrefixMapping(prefix)

    override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit =
      if !suppress(localName) then super.startElement(dropXhtml(uri), localName, qName, dropXhtml(attributes))

    override def endElement(uri: String, localName: String, qName: String): Unit =
      if !suppress(localName) then super.endElement(dropXhtml(uri), localName, qName)

    private def dropXhtml(attributes: Attributes): Attributes =
      val n: Int = attributes.getLength
      if (0 until n).forall(i => attributes.getURI(i) != XmlNamespace.xhtml.uri) then attributes else
        val copy: AttributesImpl = AttributesImpl()
        (0 until n).foreach: i =>
          copy.addAttribute(
            dropXhtml(attributes.getURI(i)),
            attributes.getLocalName(i),
            attributes.getQName(i),
            attributes.getType(i),
            attributes.getValue(i)
          )
        copy

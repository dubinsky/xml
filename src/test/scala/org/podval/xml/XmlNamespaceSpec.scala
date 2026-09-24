package org.podval.xml

import Xml.given
import ZioBlocksXml.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.schema.xml.XmlName as ZioXmlName

final class XmlNamespaceSpec extends AnyFunSuite:
  private val tei: String = "http://www.tei-c.org/ns/1.0"
  private val docbook: String = "http://docbook.org/ns/docbook"

  private def parse(xml: String): ZioBlocksXml.Element =
    XmlParser.parseXml(xml).toOption.get.to[ZioBlocksXml.Element]

  private def render(element: ZioBlocksXml.Element, config: XmlWriterConfig = XmlWriterConfig.Plain): String =
    config.render(element.to[Xml.Element])

  private def children(element: ZioBlocksXml.Element): Seq[ZioBlocksXml.Element] =
    element.getChildren.flatMap(_.asElement)

  private def attrName(element: ZioBlocksXml.Element, qualified: String): ZioXmlName =
    element.attributes.find(_._1.qualifiedName == qualified).get._1

  extension (element: ZioBlocksXml.Element)
    private def attr(attribute: XmlAttribute): Option[String] =
      element.getAttributes.collectFirst { case (n, v) if n.is(attribute) => v }

    private def attr(name: String): Option[String] =
      element.getAttributes.collectFirst { case (n, v) if n.qName == name => v }

  test("SAX: prefixed element and xmlns:prefix") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<tei:p xmlns:tei="$tei" xml:id="n1">a</tei:p>"""
    )
    assert(xml.getName.qName == "tei:p")
    assert(xml.getName.localName == XmlElement.P.localName)
    assert(xml.name.localName == "p")
    assert(xml.name.prefix.contains("tei"))
    assert(xml.name.namespace.contains(tei))
    assert(xml.attr(XmlAttribute.XmlId).contains("n1"))
    assert(xml.attr(XmlAttribute.Xmlns("tei")).contains(tei))
    val xmlId: ZioXmlName = attrName(xml, "xml:id")
    assert(xmlId.localName == "id")
    assert(xmlId.prefix.contains("xml"))
    assert(xmlId.namespace.contains(XmlNamespace.xml.uri))
    val xmlnsTei: ZioXmlName = attrName(xml, "xmlns:tei")
    assert(xmlnsTei.localName == "tei")
    assert(xmlnsTei.prefix.contains("xmlns"))
    assert(xmlnsTei.namespace.contains(XmlNamespace.xmlns.uri))
  }

  test("SAX: default namespace on the element and its children") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<article xmlns="$docbook"><title>Go</title></article>"""
    )
    assert(xml.getName.matches("article"))
    assert(xml.name.prefix.isEmpty)
    assert(xml.name.namespace.contains(docbook))
    assert(xml.attr(XmlAttribute.Xmlns).contains(docbook))
    val title: ZioBlocksXml.Element = children(xml).head
    assert(title.getName.is(XmlElement.Title))
    assert(title.name.namespace.contains(docbook))
  }

  test("SAX: nested default namespace overrides") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<outer xmlns="$docbook"><inner xmlns="$tei">x</inner></outer>"""
    )
    assert(xml.name.namespace.contains(docbook))
    val inner: ZioBlocksXml.Element = children(xml).head
    assert(inner.getName.matches("inner"))
    assert(inner.name.namespace.contains(tei))
  }

  test("SAX: unprefixed attribute has no namespace when a default namespace is in scope") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<p xmlns="$tei" n="1"/>"""
    )
    assert(xml.name.namespace.contains(tei))
    val n: ZioXmlName = attrName(xml, "n")
    assert(n.localName == "n")
    assert(n.prefix.isEmpty)
    assert(n.namespace.isEmpty)
  }

  test("SAX: prefixed attribute uses the bound namespace") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<p xmlns:xlink="${XmlNamespace.xlink.uri}" xlink:href="a.xml"/>"""
    )
    val href: ZioXmlName = attrName(xml, "xlink:href")
    assert(href.localName == "href")
    assert(href.prefix.contains("xlink"))
    assert(href.namespace.contains(XmlNamespace.xlink.uri))
  }

  test("SAX: xml: prefix is bound without an xmlns:xml declaration") {
    val xml: ZioBlocksXml.Element = parse("""<p xml:base="a.xml"/>""")
    val base: ZioXmlName = attrName(xml, "xml:base")
    assert(base.namespace.contains(XmlNamespace.xml.uri))
    assert(xml.name.namespace.isEmpty)
  }

  test("SAX: xi:include carries the XInclude namespace") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<includer>
         |  <xi:include xmlns:xi="${XmlNamespace.xinclude.uri}" href="includee.xml"/>
         |</includer>""".stripMargin
    )
    val include: ZioBlocksXml.Element = children(xml).head
    assert(include.getName.matches("include"))
    assert(include.name.prefix.contains("xi"))
    assert(include.name.namespace.contains(XmlNamespace.xinclude.uri))
  }

  test("SAX: undeclared prefix is an error") {
    val result: Either[XmlError, Xml.Element] = XmlParser.parseXml("<tei:p/>")
    assert(result.isLeft)
  }

  test("HTML parse drops the XHTML namespace") {
    val xml: ZioBlocksXml.Element = XmlParser.parseHtml("<p id=\"x\">a</p>").toOption.get.to[ZioBlocksXml.Element]
    assert(xml.getName.is(XmlElement.P))
    assert(xml.name.namespace.isEmpty)
    assert(xml.attr(XmlAttribute.Id).contains("x"))
  }

  test("parseXml keeps the XHTML namespace") {
    val xml: ZioBlocksXml.Element = parse(s"""<p xmlns="${XmlNamespace.xhtml.uri}">a</p>""")
    assert(xml.getName.is(XmlElement.P))
    assert(xml.name.namespace.contains(XmlNamespace.xhtml.uri))
  }

  test("Xml.element parses qualified names and xmlns") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(
      "tei:p",
      Seq("xmlns:tei" -> tei, "xml:id" -> "n1"),
      Seq(ZioBlocksXml.text("a"))
    )
    assert(xml.getName.qName == "tei:p")
    assert(xml.name.localName == "p")
    assert(xml.name.prefix.contains("tei"))
    assert(xml.name.namespace.contains(tei))
    assert(XmlName.asPairs(xml.getAttributes) == Seq("xmlns:tei" -> tei, "xml:id" -> "n1"))
    assert(attrName(xml, "xml:id").namespace.contains(XmlNamespace.xml.uri))
    assert(attrName(xml, "xmlns:tei").namespace.contains(XmlNamespace.xmlns.uri))
  }

  test("Xml.element default xmlns does not apply to unprefixed attributes") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(XmlElement.P.qName, Seq("xmlns" -> tei, "n" -> "1"), Seq.empty)
    assert(xml.name.namespace.contains(tei))
    assert(attrName(xml, "n").namespace.isEmpty)
    assert(attrName(xml, "xmlns").namespace.contains(XmlNamespace.xmlns.uri))
  }

  test("Xml rename and set keep an inherited namespace") {
    val child: ZioBlocksXml.Element = children(parse(s"""<outer xmlns="$tei"><inner n="1"/></outer>""")).head
    assert(child.name.namespace.contains(tei))
    val renamed: ZioBlocksXml.Element = ZioBlocksXml.withName(child, "p")
    assert(renamed.getName.is(XmlElement.P))
    assert(renamed.name.namespace.contains(tei))
    val withId: ZioBlocksXml.Element = ZioBlocksXml.withAttribute(child, XmlAttribute.XmlId.name, "n1")
    assert(withId.name.namespace.contains(tei))
    assert(withId.attr("n").contains("1"))
    assert(attrName(withId, "xml:id").namespace.contains(XmlNamespace.xml.uri))
  }

  test("ScalaXml parses qualified names and xmlns into scope") {
    val el: ScalaXml.Element = ScalaXml.element(
      "tei:p",
      Seq("xmlns:tei" -> tei, "xml:id" -> "n1"),
      Seq(ScalaXml.text("a"))
    )
    assert(el.getName.qName == "tei:p")
    assert(el.prefix == "tei")
    assert(el.label == "p")
    assert(el.scope.getURI("tei") == tei)
    assert(el.scope.getURI("xml") == XmlNamespace.xml.uri)
    assert(XmlName.asPairs(ScalaXml.getAttributes(el)) == Seq("xmlns:tei" -> tei, "xml:id" -> "n1"))
  }

  test("ScalaXml default xmlns is on scope") {
    val el: ScalaXml.Element = ScalaXml.element("article", Seq("xmlns" -> docbook), Seq.empty)
    assert(el.getName.qName == "article")
    assert(el.prefix == null)
    assert(el.scope.getURI(null) == docbook)
  }

  test("to Xml through ScalaXml keeps prefixes, xmlns, and xml: attributes") {
    val xml: ZioBlocksXml.Element = parse(s"""<tei:p xmlns:tei="$tei" xml:id="n1"><tei:hi>a</tei:hi></tei:p>""")
    val scalaEl: ScalaXml.Element = xml.to[ScalaXml.Element]
    assert(scalaEl.getName.qName == "tei:p")
    assert(scalaEl.scope.getURI("tei") == tei)
    val round: ZioBlocksXml.Element = ScalaXml.converted(scalaEl)
    assert(round.getName.qName == "tei:p")
    assert(round.name.localName == "p")
    assert(round.name.prefix.contains("tei"))
    assert(round.name.namespace.contains(tei))
    assert(round.attr(XmlAttribute.XmlId).contains("n1"))
    assert(round.attr(XmlAttribute.Xmlns("tei")).contains(tei))
    assert(children(round).map(_.getName.qName) == Seq("tei:hi"))
    assert(children(round).head.name.prefix.contains("tei"))
    assert(children(round).head.name.namespace.contains(tei))
    val scalaChild: ScalaXml.Element =
      ScalaXml.getChildren(scalaEl).flatMap(ScalaXml.asElement).head
    assert(scalaChild.scope.getURI("tei") == tei)
  }

  test("to keeps a prefixed attribute URI without xmlns on that element") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<p xmlns:xlink="${XmlNamespace.xlink.uri}"><ref xlink:href="a.xml"/></p>"""
    )
    val round: ZioBlocksXml.Element = ScalaXml.converted(xml.to[ScalaXml.Element])
    val href: ZioXmlName = attrName(children(round).head, "xlink:href")
    assert(href.namespace.contains(XmlNamespace.xlink.uri))
  }

  test("converted round-trip keeps inherited element namespace") {
    val xml: ZioBlocksXml.Element = parse(s"""<tei:p xmlns:tei="$tei"><tei:hi>a</tei:hi></tei:p>""")
    val round: ZioBlocksXml.Element = ScalaXml.converted(xml.to[ScalaXml.Element])
    assert(round.name.namespace.contains(tei))
    assert(children(round).head.name.namespace.contains(tei))
  }

  test("set keeps inherited namespace") {
    val child: ZioBlocksXml.Element = children(parse(s"""<outer xmlns="$tei"><inner n="1"/></outer>""")).head
    val updated: ZioBlocksXml.Element = ZioBlocksXml.withAttribute(child, XmlAttribute.XmlId.name, "n1")
    assert(updated.name.namespace.contains(tei))
    assert(updated.attr("n").contains("1"))
    assert(attrName(updated, "n").namespace.isEmpty)
  }

  test("XmlName xmlns and xml tests") {
    val xmlnsTei: XmlName = XmlName.parse("xmlns:tei", isAttribute = true)
    assert(xmlnsTei.isXmlnsDeclaration)
    assert(!xmlnsTei.isDefaultXmlns)
    val xmlnsDefault: XmlName = XmlName.parse("xmlns", isAttribute = true)
    assert(xmlnsDefault.isDefaultXmlns)
    assert(xmlnsDefault.isXmlnsDeclaration)
    val xmlId: XmlName = XmlName.parse("xml:id", isAttribute = true)
    assert(xmlId.isXml)
    assert(!xmlId.isXmlnsDeclaration)
    assert(xmlId.sameAs(XmlName("id", Some(XmlNamespace.xml))))
    assert(xmlId.sameAs(XmlAttribute.XmlId))
    assert(xmlId.is(XmlAttribute.XmlId))
    assert(XmlAttribute.XmlId.matches(xmlId))
    assert(!xmlId.is(XmlAttribute.Id))
    assert(!XmlName("id").is(XmlAttribute.XmlId))
    assert(XmlName("p").is(XmlElement.P))
    assert(XmlElement.P.matches(XmlName("p")))
    assert(!XmlName.parse("tei:p").is(XmlElement.P))
    assert(XmlName.parse("tei:p").matches("p"))
    assert(XmlName.parse("tei:p").matches("tei:p"))
    assert(!XmlName("p").matches("tei:p"))
    assert(!XmlName.parse("html:p").matches("tei:p"))
    assert(XmlName.parse("tei:p").matchesAny(Seq("div", "p")))
    assert(XmlName.parse("tei:p").localNameIn(Set("p", "div")))
    assert(XmlName.parse("xi:include").isInclude)
    assert(!XmlName("include").isInclude)
  }

  test("writer does not repeat an in-scope default xmlns") {
    val xml: ZioBlocksXml.Element = parse(s"""<outer xmlns="$tei"><inner n="1">x</inner></outer>""")
    val dumped: String = render(xml)
    assert(dumped.contains(s"""xmlns="$tei""""), dumped)
    assert(dumped.indexOf("xmlns=") == dumped.lastIndexOf("xmlns="), dumped)
  }

  test("writer does not repeat an in-scope prefixed xmlns") {
    val xml: ZioBlocksXml.Element = parse(s"""<tei:p xmlns:tei="$tei"><tei:hi>a</tei:hi></tei:p>""")
    val dumped: String = render(xml)
    assert(dumped.contains("<tei:hi>"), dumped)
    assert(dumped.indexOf("xmlns:tei=") == dumped.lastIndexOf("xmlns:tei="), dumped)
  }

  test("writer keeps an author-written xmlns undeclare") {
    val xml: ZioBlocksXml.Element = parse(s"""<outer xmlns="$tei"><inner xmlns="">x</inner></outer>""")
    val dumped: String = render(xml)
    assert(dumped.contains("""<inner xmlns="">"""), dumped)
  }

  test("writer does not repeat xmlns on a preformatted child") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<article xmlns="$docbook"><programlisting><co id="x"/></programlisting></article>"""
    )
    val dumped: String = render(xml, XmlWriterConfig(preformat = Set("programlisting")))
    assert(dumped.contains("<co"), dumped)
    assert(dumped.indexOf("xmlns=") == dumped.lastIndexOf("xmlns="), dumped)
  }

  test("a small namespaced document is idempotent") {
    val xml: ZioBlocksXml.Element = parse(s"""<tei:p xmlns:tei="$tei" xml:id="n1"><tei:hi>a</tei:hi></tei:p>""")
    val once: String = render(xml)
    val twice: String = render(parse(once.trim))
    assert(twice == once, twice)
  }

  test("writer emits xmlns for an inherited namespace on a child written alone") {
    val child: ZioBlocksXml.Element = children(parse(s"""<outer xmlns="$tei"><inner n="1"/></outer>""")).head
    assert(child.name.namespace.contains(tei))
    assert(child.attr(XmlAttribute.Xmlns).isEmpty)
    val dumped: String = render(child)
    assert(dumped.contains("xmlns="), dumped)
    assert(dumped.contains(tei), dumped)
    assert(dumped.contains("<inner"), dumped)
  }

  test("writer emits xmlns and prefixed names") {
    val xml: ZioBlocksXml.Element = parse(s"""<tei:p xmlns:tei="$tei" xml:id="n1">a</tei:p>""")
    val dumped: String = render(xml)
    assert(dumped.contains("xmlns:tei="), dumped)
    assert(dumped.contains(tei), dumped)
    assert(dumped.contains("<tei:p"), dumped)
    assert(dumped.contains("</tei:p>"), dumped)
    assert(dumped.contains("xml:id="), dumped)
  }

  test("parse-write-parse keeps namespace names") {
    val xml: ZioBlocksXml.Element = parse(
      s"""<tei:p xmlns:tei="$tei" xml:id="n1"><tei:hi>a</tei:hi></tei:p>"""
    )
    val round: ZioBlocksXml.Element = parse(render(xml).trim)
    assert(round.name.namespace.contains(tei))
    assert(round.attr(XmlAttribute.XmlId).contains("n1"))
    assert(children(round).head.name.namespace.contains(tei))
  }

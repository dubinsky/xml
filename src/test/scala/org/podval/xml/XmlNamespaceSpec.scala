package org.podval.xml

import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.schema.xml.XmlName

final class XmlNamespaceSpec extends AnyFunSuite:
  private val tei: String = "http://www.tei-c.org/ns/1.0"
  private val docbook: String = "http://docbook.org/ns/docbook"

  private def parse(xml: String): Xml.Element =
    XmlParser.parseXml(xml).toOption.get

  private def children(element: Xml.Element): Seq[Xml.Element] =
    element.getChildren.flatMap(_.asElement)

  private def attrName(element: Xml.Element, qualified: String): XmlName =
    element.attributes.find(_._1.qualifiedName == qualified).get._1

  test("SAX: prefixed element and xmlns:prefix") {
    val xml: Xml.Element = parse(
      s"""<tei:p xmlns:tei="$tei" xml:id="n1">a</tei:p>"""
    )
    assert(xml.getName == "tei:p")
    assert(xml.localName == "p")
    assert(xml.name.localName == "p")
    assert(xml.name.prefix.contains("tei"))
    assert(xml.name.namespace.contains(tei))
    assert(xml.get(XmlAttribute.XmlId).contains("n1"))
    assert(xml.get(XmlAttribute.Xmlns("tei")).contains(tei))
    val xmlId: XmlName = attrName(xml, "xml:id")
    assert(xmlId.localName == "id")
    assert(xmlId.prefix.contains("xml"))
    assert(xmlId.namespace.contains(XmlNamespace.xml.uri))
    val xmlnsTei: XmlName = attrName(xml, "xmlns:tei")
    assert(xmlnsTei.localName == "tei")
    assert(xmlnsTei.prefix.contains("xmlns"))
    assert(xmlnsTei.namespace.contains(XmlNamespace.xmlns.uri))
  }

  test("SAX: default namespace on the element and its children") {
    val xml: Xml.Element = parse(
      s"""<article xmlns="$docbook"><title>Go</title></article>"""
    )
    assert(xml.getName == "article")
    assert(xml.name.prefix.isEmpty)
    assert(xml.name.namespace.contains(docbook))
    assert(xml.get(XmlAttribute.Xmlns).contains(docbook))
    val title: Xml.Element = children(xml).head
    assert(title.getName == "title")
    assert(title.name.namespace.contains(docbook))
  }

  test("SAX: nested default namespace overrides") {
    val xml: Xml.Element = parse(
      s"""<outer xmlns="$docbook"><inner xmlns="$tei">x</inner></outer>"""
    )
    assert(xml.name.namespace.contains(docbook))
    val inner: Xml.Element = children(xml).head
    assert(inner.getName == "inner")
    assert(inner.name.namespace.contains(tei))
  }

  test("SAX: unprefixed attribute has no namespace when a default namespace is in scope") {
    val xml: Xml.Element = parse(
      s"""<p xmlns="$tei" n="1"/>"""
    )
    assert(xml.name.namespace.contains(tei))
    val n: XmlName = attrName(xml, "n")
    assert(n.localName == "n")
    assert(n.prefix.isEmpty)
    assert(n.namespace.isEmpty)
  }

  test("SAX: prefixed attribute uses the bound namespace") {
    val xml: Xml.Element = parse(
      s"""<p xmlns:xlink="${XmlNamespace.xlink.uri}" xlink:href="a.xml"/>"""
    )
    val href: XmlName = attrName(xml, "xlink:href")
    assert(href.localName == "href")
    assert(href.prefix.contains("xlink"))
    assert(href.namespace.contains(XmlNamespace.xlink.uri))
  }

  test("SAX: xml: prefix is bound without an xmlns:xml declaration") {
    val xml: Xml.Element = parse("""<p xml:base="a.xml"/>""")
    val base: XmlName = attrName(xml, "xml:base")
    assert(base.namespace.contains(XmlNamespace.xml.uri))
    assert(xml.name.namespace.isEmpty)
  }

  test("SAX: xi:include carries the XInclude namespace") {
    val xml: Xml.Element = parse(
      s"""<includer>
         |  <xi:include xmlns:xi="${XmlNamespace.xinclude.uri}" href="includee.xml"/>
         |</includer>""".stripMargin
    )
    val include: Xml.Element = children(xml).head
    assert(include.localName == "include")
    assert(include.name.prefix.contains("xi"))
    assert(include.name.namespace.contains(XmlNamespace.xinclude.uri))
  }

  test("SAX: undeclared prefix is an error") {
    val result: Either[Throwable, Xml.Element] = XmlParser.parseXml("<tei:p/>")
    assert(result.isLeft)
  }

  test("HTML parse drops the XHTML namespace") {
    val xml: Xml.Element = XmlParser.parseHtml("<p id=\"x\">a</p>").toOption.get
    assert(xml.getName == "p")
    assert(xml.name.namespace.isEmpty)
    assert(xml.get(XmlAttribute.Id).contains("x"))
  }

  test("parseXml keeps the XHTML namespace") {
    val xml: Xml.Element = parse(s"""<p xmlns="${XmlNamespace.xhtml.uri}">a</p>""")
    assert(xml.getName == "p")
    assert(xml.name.namespace.contains(XmlNamespace.xhtml.uri))
  }

  test("Xml.element parses qualified names and xmlns") {
    val xml: Xml.Element = Xml.element(
      "tei:p",
      Seq("xmlns:tei" -> tei, "xml:id" -> "n1"),
      Seq(Xml.text("a"))
    )
    assert(xml.getName == "tei:p")
    assert(xml.name.localName == "p")
    assert(xml.name.prefix.contains("tei"))
    assert(xml.name.namespace.contains(tei))
    assert(xml.getAttributes == Seq("xmlns:tei" -> tei, "xml:id" -> "n1"))
    assert(attrName(xml, "xml:id").namespace.contains(XmlNamespace.xml.uri))
    assert(attrName(xml, "xmlns:tei").namespace.contains(XmlNamespace.xmlns.uri))
  }

  test("Xml.element default xmlns does not apply to unprefixed attributes") {
    val xml: Xml.Element = Xml.element("p", Seq("xmlns" -> tei, "n" -> "1"), Seq.empty)
    assert(xml.name.namespace.contains(tei))
    assert(attrName(xml, "n").namespace.isEmpty)
    assert(attrName(xml, "xmlns").namespace.contains(XmlNamespace.xmlns.uri))
  }

  test("Xml rename and setAttributes keep an inherited namespace") {
    val child: Xml.Element = children(parse(s"""<outer xmlns="$tei"><inner n="1"/></outer>""")).head
    assert(child.name.namespace.contains(tei))
    val renamed: Xml.Element = child.rename("p")
    assert(renamed.getName == "p")
    assert(renamed.name.namespace.contains(tei))
    val withId: Xml.Element = child.set(XmlAttribute.XmlId, "n1")
    assert(withId.name.namespace.contains(tei))
    assert(withId.get("n").contains("1"))
    assert(attrName(withId, "xml:id").namespace.contains(XmlNamespace.xml.uri))
  }

  test("ScalaXml parses qualified names and xmlns into scope") {
    val el: ScalaXml.Element = ScalaXml.element(
      "tei:p",
      Seq("xmlns:tei" -> tei, "xml:id" -> "n1"),
      Seq(ScalaXml.text("a"))
    )
    assert(ScalaXml.getName(el) == "tei:p")
    assert(el.prefix == "tei")
    assert(el.label == "p")
    assert(el.scope.getURI("tei") == tei)
    assert(el.scope.getURI("xml") == XmlNamespace.xml.uri)
    assert(ScalaXml.getAttributes(el) == Seq("xmlns:tei" -> tei, "xml:id" -> "n1"))
  }

  test("ScalaXml default xmlns is on scope") {
    val el: ScalaXml.Element = ScalaXml.element("article", Seq("xmlns" -> docbook), Seq.empty)
    assert(ScalaXml.getName(el) == "article")
    assert(el.prefix == null)
    assert(el.scope.getURI(null) == docbook)
  }

  test("to Xml through ScalaXml keeps prefixes, xmlns, and xml: attributes") {
    val xml: Xml.Element = parse(s"""<tei:p xmlns:tei="$tei" xml:id="n1"><tei:hi>a</tei:hi></tei:p>""")
    val scalaEl: ScalaXml.Element = xml.to[ScalaXml.Element]
    assert(ScalaXml.getName(scalaEl) == "tei:p")
    assert(scalaEl.scope.getURI("tei") == tei)
    val round: Xml.Element = ScalaXml.converted(scalaEl)
    assert(round.getName == "tei:p")
    assert(round.name.localName == "p")
    assert(round.name.prefix.contains("tei"))
    assert(round.name.namespace.contains(tei))
    assert(round.get(XmlAttribute.XmlId).contains("n1"))
    assert(round.get(XmlAttribute.Xmlns("tei")).contains(tei))
    assert(children(round).map(_.getName) == Seq("tei:hi"))
    assert(children(round).head.name.prefix.contains("tei"))
    assert(children(round).head.name.namespace.contains(tei))
    val scalaChild: ScalaXml.Element =
      ScalaXml.getChildren(scalaEl).flatMap(ScalaXml.asElement).head
    assert(scalaChild.scope.getURI("tei") == tei)
  }

  test("to keeps a prefixed attribute URI without xmlns on that element") {
    val xml: Xml.Element = parse(
      s"""<p xmlns:xlink="${XmlNamespace.xlink.uri}"><ref xlink:href="a.xml"/></p>"""
    )
    val round: Xml.Element = ScalaXml.converted(xml.to[ScalaXml.Element])
    val href: XmlName = attrName(children(round).head, "xlink:href")
    assert(href.namespace.contains(XmlNamespace.xlink.uri))
  }

  test("converted round-trip keeps inherited element namespace") {
    val xml: Xml.Element = parse(s"""<tei:p xmlns:tei="$tei"><tei:hi>a</tei:hi></tei:p>""")
    val round: Xml.Element = ScalaXml.converted(xml.to[ScalaXml.Element])
    assert(round.name.namespace.contains(tei))
    assert(children(round).head.name.namespace.contains(tei))
  }

  test("set keeps inherited namespace") {
    val child: Xml.Element = children(parse(s"""<outer xmlns="$tei"><inner n="1"/></outer>""")).head
    val updated: Xml.Element = child.set(XmlAttribute.XmlId, "n1")
    assert(updated.name.namespace.contains(tei))
    assert(updated.get("n").contains("1"))
    assert(attrName(updated, "n").namespace.isEmpty)
  }

  test("XmlExpandedName xmlns and xml tests") {
    val xmlnsTei: XmlExpandedName = XmlExpandedName.parse("xmlns:tei", isAttribute = true)
    assert(xmlnsTei.isXmlnsDeclaration)
    assert(!xmlnsTei.isDefaultXmlns)
    val xmlnsDefault: XmlExpandedName = XmlExpandedName.parse("xmlns", isAttribute = true)
    assert(xmlnsDefault.isDefaultXmlns)
    assert(xmlnsDefault.isXmlnsDeclaration)
    val xmlId: XmlExpandedName = XmlExpandedName.parse("xml:id", isAttribute = true)
    assert(xmlId.isXml)
    assert(!xmlId.isXmlnsDeclaration)
    assert(xmlId.sameAs(XmlExpandedName("id", Some("xml"), Some(XmlNamespace.xml.uri))))
  }

  test("writer emits xmlns for an inherited namespace on a child written alone") {
    val child: Xml.Element = children(parse(s"""<outer xmlns="$tei"><inner n="1"/></outer>""")).head
    assert(child.name.namespace.contains(tei))
    assert(child.get(XmlAttribute.Xmlns).isEmpty)
    val dumped: String = XmlWriterConfig.Plain.render(child)
    assert(dumped.contains("xmlns="), dumped)
    assert(dumped.contains(tei), dumped)
    assert(dumped.contains("<inner"), dumped)
  }

  test("writer emits xmlns and prefixed names") {
    val xml: Xml.Element = parse(s"""<tei:p xmlns:tei="$tei" xml:id="n1">a</tei:p>""")
    val dumped: String = XmlWriterConfig.Plain.render(xml)
    assert(dumped.contains("xmlns:tei="), dumped)
    assert(dumped.contains(tei), dumped)
    assert(dumped.contains("<tei:p"), dumped)
    assert(dumped.contains("</tei:p>"), dumped)
    assert(dumped.contains("xml:id="), dumped)
  }

  test("parse-write-parse keeps namespace names") {
    val xml: Xml.Element = parse(
      s"""<tei:p xmlns:tei="$tei" xml:id="n1"><tei:hi>a</tei:hi></tei:p>"""
    )
    val round: Xml.Element = parse(XmlWriterConfig.Plain.render(xml).trim)
    assert(round.name.namespace.contains(tei))
    assert(round.get(XmlAttribute.XmlId).contains("n1"))
    assert(children(round).head.name.namespace.contains(tei))
  }

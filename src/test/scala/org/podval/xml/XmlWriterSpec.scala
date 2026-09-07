package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class XmlWriterSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    XmlWriterConfig.Plain.render(element)

  test("CDATA is written as a CDATA section") {
    val xml: Xml.Element = Xml.element("p", Seq.empty, Seq(Xml.cdata("a<b")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a<b]]>"), dumped)
    assert(!dumped.contains("a&lt;b"), dumped)
  }

  test("mixed text and CDATA keep the CDATA section") {
    val xml: Xml.Element = Xml.element(
      "p",
      Seq.empty,
      Seq(Xml.text("a"), Xml.cdata("b"), Xml.text("c"))
    )
    val dumped: String = render(xml)
    assert(dumped.contains("a<![CDATA[b]]>c") || dumped.contains("a<![CDATA[b]]> c"), dumped)
    assert(dumped.contains("<![CDATA[b]]>"), dumped)
  }

  test("adjacent CDATA sections stay two sections") {
    val xml: Xml.Element = Xml.element(
      "p",
      Seq.empty,
      Seq(Xml.cdata("a"), Xml.cdata("b"))
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a]]>"), dumped)
    assert(dumped.contains("<![CDATA[b]]>"), dumped)
  }

  test("]]> inside CDATA is split into legal sections") {
    val xml: Xml.Element = Xml.element("p", Seq.empty, Seq(Xml.cdata("a]]>b")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a]]]]><![CDATA[>b]]>"), dumped)
  }

  test("whitespace-only CDATA is not dropped") {
    val xml: Xml.Element = Xml.element("p", Seq.empty, Seq(Xml.cdata("  ")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[  ]]>"), dumped)
  }

  test("SAX round-trip keeps CDATA syntax") {
    val reader = javax.xml.parsers.SAXParserFactory.newInstance.newSAXParser.getXMLReader
    val xml: Xml.Element = XmlParserSax.parse(reader, "<p>a<![CDATA[b<c]]>d</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[b<c]]>"), dumped)
  }

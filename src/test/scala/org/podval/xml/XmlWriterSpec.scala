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

  test("parseXml round-trip keeps CDATA syntax") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<![CDATA[b<c]]>d</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[b<c]]>"), dumped)
  }

  test("comment is written as a comment") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<!--c-->b</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<!--c-->"), dumped)
    assert(dumped.contains("a<!--c-->b") || dumped.contains("a<!--c--> b"), dumped)
  }

  test("processing instruction is written as a PI") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<?pi d?>b</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<?pi d?>"), dumped)
  }

  test("parseXml round-trip keeps comments and processing instructions") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<!--c--><?pi d?>b</p>").toOption.get
    val dumped: String = render(xml)
    val round: Xml.Element = XmlParser.parseXml(dumped).toOption.get
    assert(round.getChildren.flatMap(_.asComment) == Seq("c"), dumped)
    assert(round.getChildren.flatMap(_.asProcessingInstruction) == Seq(("pi", "d")), dumped)
    assert(round.getChildren.flatMap(_.asText) == Seq("a", "b"), dumped)
  }

  test("comment and PI inside preformat") {
    val xml: Xml.Element = XmlParser.parseXml("<pre>a<!--c--><?pi d?>b</pre>").toOption.get
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(xml)
    assert(dumped.contains("<!--c-->"), dumped)
    assert(dumped.contains("<?pi d?>"), dumped)
  }

  test("text encodes & and <") {
    val dumped: String = render(Xml.element("p").setText("a & b < c"))
    assert(dumped.contains("a &amp; b &lt; c"), dumped)
  }

  test("text does not encode an existing entity") {
    val dumped: String = render(Xml.element("p").setText("a&nbsp;b"))
    assert(dumped.contains("a&nbsp;b"), dumped)
    assert(!dumped.contains("&amp;nbsp;"), dumped)
  }

  test("parsed &amp; round-trips; parsed &lt; is decoded then encoded") {
    val amp: String = render(XmlParser.parseXml("<p>a&amp;b</p>").toOption.get)
    assert(amp.contains("a&amp;b"), amp)
    val lt: Xml.Element = XmlParser.parseXml("<p>a&lt;b</p>").toOption.get
    assert(lt.getChildren.flatMap(_.asText).mkString == "a<b")
    assert(render(lt).contains("a&lt;b"))
  }

  test("parsed undeclared &nbsp; is written as &nbsp;") {
    val dumped: String = render(XmlParser.parseXml("<p>a&nbsp;b</p>").toOption.get)
    assert(dumped.contains("a&nbsp;b"), dumped)
    assert(!dumped.contains("&amp;nbsp;"), dumped)
  }

  test("CDATA is not entity-encoded") {
    val dumped: String = render(Xml.element("p", Seq.empty, Seq(Xml.cdata("a&b<c"))))
    assert(dumped.contains("<![CDATA[a&b<c]]>"), dumped)
  }

  test("attribute encodes &, <, and \"") {
    val dumped: String = render(Xml.element("p").set(XmlAttribute.Title, "a & b < \"c\""))
    assert(dumped.contains("a &amp; b &lt; &quot;c&quot;"), dumped)
  }

  test("preformat encodes & and < but not &nbsp;") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      Xml.element("pre").setText("a & b < c &nbsp; d")
    )
    assert(dumped.contains("a &amp; b &lt; c &nbsp; d"), dumped)
  }

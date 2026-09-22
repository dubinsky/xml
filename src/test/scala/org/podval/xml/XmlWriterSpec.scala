package org.podval.xml

import ZioBlocksXml.given
import org.scalatest.funsuite.AnyFunSuite

final class XmlWriterSpec extends AnyFunSuite:
  private def render(element: ZioBlocksXml.Element): String =
    XmlWriterConfig.Plain.render(element)

  test("CDATA is written as a CDATA section") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(XmlElement.P.qName, Seq.empty, Seq(ZioBlocksXml.cdata("a<b")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a<b]]>"), dumped)
    assert(!dumped.contains("a&lt;b"), dumped)
  }

  test("mixed text and CDATA keep the CDATA section") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(
      "p",
      Seq.empty,
      Seq(ZioBlocksXml.text("a"), ZioBlocksXml.cdata("b"), ZioBlocksXml.text("c"))
    )
    val dumped: String = render(xml)
    assert(dumped.contains("a<![CDATA[b]]>c") || dumped.contains("a<![CDATA[b]]> c"), dumped)
    assert(dumped.contains("<![CDATA[b]]>"), dumped)
  }

  test("adjacent CDATA sections stay two sections") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(
      "p",
      Seq.empty,
      Seq(ZioBlocksXml.cdata("a"), ZioBlocksXml.cdata("b"))
    )
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a]]>"), dumped)
    assert(dumped.contains("<![CDATA[b]]>"), dumped)
  }

  test("]]> inside CDATA is split into legal sections") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(XmlElement.P.qName, Seq.empty, Seq(ZioBlocksXml.cdata("a]]>b")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a]]]]><![CDATA[>b]]>"), dumped)
  }

  test("whitespace-only CDATA is not dropped") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(XmlElement.P.qName, Seq.empty, Seq(ZioBlocksXml.cdata("  ")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[  ]]>"), dumped)
  }

  test("parseXml round-trip keeps CDATA syntax") {
    val xml: ZioBlocksXml.Element = XmlParser.parseXml("<p>a<![CDATA[b<c]]>d</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[b<c]]>"), dumped)
  }

  test("comment is written as a comment") {
    val xml: ZioBlocksXml.Element = XmlParser.parseXml("<p>a<!--c-->b</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<!--c-->"), dumped)
    assert(dumped.contains("a<!--c-->b") || dumped.contains("a<!--c--> b"), dumped)
  }

  test("processing instruction is written as a PI") {
    val xml: ZioBlocksXml.Element = XmlParser.parseXml("<p>a<?pi d?>b</p>").toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<?pi d?>"), dumped)
  }

  test("parseXml round-trip keeps comments and processing instructions") {
    val xml: ZioBlocksXml.Element = XmlParser.parseXml("<p>a<!--c--><?pi d?>b</p>").toOption.get
    val dumped: String = render(xml)
    val round: ZioBlocksXml.Element = XmlParser.parseXml(dumped).toOption.get
    assert(round.getChildren.flatMap(_.asComment) == Seq("c"), dumped)
    assert(round.getChildren.flatMap(_.asProcessingInstruction) == Seq(("pi", "d")), dumped)
    assert(round.getChildren.flatMap(_.asText) == Seq("a", "b"), dumped)
  }

  test("comment and PI inside preformat") {
    val xml: ZioBlocksXml.Element = XmlParser.parseXml("<pre>a<!--c--><?pi d?>b</pre>").toOption.get
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(xml)
    assert(dumped.contains("<!--c-->"), dumped)
    assert(dumped.contains("<?pi d?>"), dumped)
  }

  test("break forces a newline after the element when a right-side break is allowed") {
    val xml: ZioBlocksXml.Element = XmlParser.parseXml("<l>foo<lb/>\nbar</l>").toOption.get
    val dumped: String = XmlWriterConfig(break = Set("lb"), cling = Set("lb")).render(xml, 120)
    assert(dumped.contains("<lb></lb>\n"), dumped)
    assert(!dumped.contains("<lb></lb> bar"), dumped)
  }

  test("text encodes & and <") {
    val dumped: String = render(ZioBlocksXml.element(XmlElement.P).setText("a & b < c"))
    assert(dumped.contains("a &amp; b &lt; c"), dumped)
  }

  test("text does not encode an existing entity") {
    val dumped: String = render(ZioBlocksXml.element(XmlElement.P).setText("a&nbsp;b"))
    assert(dumped.contains("a&nbsp;b"), dumped)
    assert(!dumped.contains("&amp;nbsp;"), dumped)
  }

  test("parsed &amp; round-trips; parsed &lt; is decoded then encoded") {
    val amp: String = render(XmlParser.parseXml("<p>a&amp;b</p>").toOption.get)
    assert(amp.contains("a&amp;b"), amp)
    val lt: ZioBlocksXml.Element = XmlParser.parseXml("<p>a&lt;b</p>").toOption.get
    assert(lt.getChildren.flatMap(_.asText).mkString == "a<b")
    assert(render(lt).contains("a&lt;b"))
  }

  test("parsed undeclared &nbsp; is written as &nbsp;") {
    val dumped: String = render(XmlParser.parseXml("<p>a&nbsp;b</p>").toOption.get)
    assert(dumped.contains("a&nbsp;b"), dumped)
    assert(!dumped.contains("&amp;nbsp;"), dumped)
  }

  test("CDATA is not entity-encoded") {
    val dumped: String = render(ZioBlocksXml.element(XmlElement.P.qName, Seq.empty, Seq(ZioBlocksXml.cdata("a&b<c"))))
    assert(dumped.contains("<![CDATA[a&b<c]]>"), dumped)
  }

  test("attribute encodes &, <, and \"") {
    val dumped: String = render(ZioBlocksXml.element(XmlElement.P).set(XmlAttribute.Title, "a & b < \"c\""))
    assert(dumped.contains("a &amp; b &lt; &quot;c&quot;"), dumped)
  }

  test("preformat encodes & and < but not &nbsp;") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      ZioBlocksXml.element(XmlElement.Pre).setText("a & b < c &nbsp; d")
    )
    assert(dumped.contains("a &amp; b &lt; c &nbsp; d"), dumped)
  }

  test("preformat attributes are space-separated") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      ZioBlocksXml.element(XmlElement.Pre).set("class", "x").set("id", "y").setText("a")
    )
    assert(
      dumped.contains("""<pre class="x" id="y">""") || dumped.contains("""<pre id="y" class="x">"""),
      dumped
    )
    assert(!dumped.contains(","), dumped)
  }

  test("preformat empty non-void keeps an end tag") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(ZioBlocksXml.element("pre"))
    assert(dumped.contains("<pre></pre>"), dumped)
    assert(!dumped.contains("<pre/>"), dumped)
  }

  test("preformat empty void self-closes") {
    val dumped: String = XmlWriterConfig(preformat = Set("br"), selfClose = Set("br")).render(
      ZioBlocksXml.element("br")
    )
    assert(dumped.contains("<br/>"), dumped)
    assert(!dumped.contains("<br></br>"), dumped)
  }

  test("preformat keeps a literal backslash-n") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      ZioBlocksXml.element(XmlElement.Pre).setText("a\\nb")
    )
    assert(dumped.contains("a\\nb"), dumped)
    val inner: String = dumped.substring(dumped.indexOf('>') + 1, dumped.lastIndexOf('<'))
    assert(!inner.contains('\n'), inner)
  }

  test("plain writer still encodes <script> text") {
    val dumped: String = render(ZioBlocksXml.element(XmlElement.Script).setText("a < b && c"))
    assert(dumped.contains("a &lt; b &amp;&amp; c"), dumped)
    assert(!dumped.contains("a < b"), dumped)
  }

  test("custom rawText does not encode script text") {
    val dumped: String = XmlWriterConfig(rawText = Set("script")).render(
      ZioBlocksXml.element(XmlElement.Script).setText("a < b && c")
    )
    assert(dumped.contains("a < b && c"), dumped)
    assert(!dumped.contains("&lt;"), dumped)
    assert(!dumped.contains("&amp;"), dumped)
  }

  test("rawText wins dual membership with preformat") {
    val dumped: String = XmlWriterConfig(
      preformat = Set("script"),
      rawText = Set("script")
    ).render(ZioBlocksXml.element(XmlElement.Script).setText("a < b"))
    assert(dumped.contains("a < b"), dumped)
    assert(!dumped.contains("&lt;"), dumped)
  }

  test("plus unions rawText") {
    val combined: XmlWriterConfig = HtmlXmlWriterConfig.plus(XmlWriterConfig.Plain)
    assert(combined.rawText.contains(XmlElement.Script.localName))
    assert(combined.rawText.contains(XmlElement.Style.localName))
    val fromPlain: XmlWriterConfig = XmlWriterConfig.Plain.plus(HtmlXmlWriterConfig)
    assert(fromPlain.rawText.contains(XmlElement.Script.localName))
  }

  test("preformat concatenates adjacent text") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      ZioBlocksXml.element(XmlElement.Pre).setChildren(Seq(ZioBlocksXml.text("a"), ZioBlocksXml.text("b")))
    )
    assert(dumped.contains(">ab<"), dumped)
    assert(!dumped.contains("a\nb"), dumped)
  }

  test("plain CDATA in script stays a CDATA section") {
    val dumped: String = render(
      ZioBlocksXml.element(XmlElement.Script.qName, Seq.empty, Seq(ZioBlocksXml.cdata("a < b")))
    )
    assert(dumped.contains("<![CDATA[a < b]]>"), dumped)
  }

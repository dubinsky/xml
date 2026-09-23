package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class XmlWriterSpec extends AnyFunSuite:
  private def render(element: Xml.Element): String =
    XmlWriterConfig.Plain.render(element)

  test("CDATA is written as a CDATA section") {
    val xml: Xml.Element = Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("a<b")))
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
    val xml: Xml.Element = Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("a]]>b")))
    val dumped: String = render(xml)
    assert(dumped.contains("<![CDATA[a]]]]><![CDATA[>b]]>"), dumped)
  }

  test("whitespace-only CDATA is not dropped") {
    val xml: Xml.Element = Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("  ")))
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

  test("break forces a newline after the element when a right-side break is allowed") {
    val xml: Xml.Element = XmlParser.parseXml("<l>foo<lb/>\nbar</l>").toOption.get
    val dumped: String = XmlWriterConfig(break = Set("lb"), cling = Set("lb")).render(xml, 120)
    assert(dumped.contains("<lb></lb>\n"), dumped)
    assert(!dumped.contains("<lb></lb> bar"), dumped)
  }

  test("text encodes & and <") {
    val dumped: String = render(Xml.element(XmlElement.P).setText("a & b < c"))
    assert(dumped.contains("a &amp; b &lt; c"), dumped)
  }

  test("text does not encode an existing entity") {
    val dumped: String = render(Xml.element(XmlElement.P).setText("a&nbsp;b"))
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
    val dumped: String = render(Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("a&b<c"))))
    assert(dumped.contains("<![CDATA[a&b<c]]>"), dumped)
  }

  test("attribute encodes &, <, and \"") {
    val dumped: String = render(Xml.element(XmlElement.P).set(XmlAttribute.Title, "a & b < \"c\""))
    assert(dumped.contains("a &amp; b &lt; &quot;c&quot;"), dumped)
  }

  test("preformat encodes & and < but not &nbsp;") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      Xml.element(XmlElement.Pre).setText("a & b < c &nbsp; d")
    )
    assert(dumped.contains("a &amp; b &lt; c &nbsp; d"), dumped)
  }

  test("preformat attributes are space-separated") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      Xml.element(XmlElement.Pre).set("class", "x").set("id", "y").setText("a")
    )
    assert(
      dumped.contains("""<pre class="x" id="y">""") || dumped.contains("""<pre id="y" class="x">"""),
      dumped
    )
    assert(!dumped.contains(","), dumped)
  }

  test("preformat empty non-void keeps an end tag") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(Xml.element("pre"))
    assert(dumped.contains("<pre></pre>"), dumped)
    assert(!dumped.contains("<pre/>"), dumped)
  }

  test("preformat empty void self-closes") {
    val dumped: String = XmlWriterConfig(preformat = Set("br"), selfClose = Set("br")).render(
      Xml.element("br")
    )
    assert(dumped.contains("<br/>"), dumped)
    assert(!dumped.contains("<br></br>"), dumped)
  }

  test("preformat keeps a literal backslash-n") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      Xml.element(XmlElement.Pre).setText("a\\nb")
    )
    assert(dumped.contains("a\\nb"), dumped)
    val inner: String = dumped.substring(dumped.indexOf('>') + 1, dumped.lastIndexOf('<'))
    assert(!inner.contains('\n'), inner)
  }

  test("plain writer still encodes <script> text") {
    val dumped: String = render(Xml.element(XmlElement.Script).setText("a < b && c"))
    assert(dumped.contains("a &lt; b &amp;&amp; c"), dumped)
    assert(!dumped.contains("a < b"), dumped)
  }

  test("custom rawText does not encode script text") {
    val dumped: String = XmlWriterConfig(rawText = Set("script")).render(
      Xml.element(XmlElement.Script).setText("a < b && c")
    )
    assert(dumped.contains("a < b && c"), dumped)
    assert(!dumped.contains("&lt;"), dumped)
    assert(!dumped.contains("&amp;"), dumped)
  }

  test("rawText wins dual membership with preformat") {
    val dumped: String = XmlWriterConfig(
      preformat = Set("script"),
      rawText = Set("script")
    ).render(Xml.element(XmlElement.Script).setText("a < b"))
    assert(dumped.contains("a < b"), dumped)
    assert(!dumped.contains("&lt;"), dumped)
  }

  test("selfCloseEmpty writes empty tags and leaves a non-empty element paired") {
    val config: XmlWriterConfig = XmlWriterConfig(selfCloseEmpty = true)
    val name: String = config.render(Xml.element("name", Seq("n" -> "a"), Seq.empty))
    assert(name.contains("""<name n="a"/>"""), name)
    val lb: String = config.render(Xml.element("lb"))
    assert(lb.contains("<lb/>"), lb)
    assert(!lb.contains("</lb>"), lb)
    val paragraph: String = config.render(Xml.element("p").setText("text"))
    assert(paragraph.contains("<p>text</p>"), paragraph)
    assert(!paragraph.contains("<p/>"), paragraph)
  }

  test("HtmlXmlWriterConfig still pairs an empty script and self-closes br") {
    val script: String = HtmlXmlWriterConfig.render(Xml.element("script"))
    assert(script.contains("<script></script>"), script)
    assert(!script.contains("<script/>"), script)
    val br: String = HtmlXmlWriterConfig.render(Xml.element("br"))
    assert(br.contains("<br/>"), br)
  }

  test("preformat concatenates adjacent text") {
    val dumped: String = XmlWriterConfig(preformat = Set("pre")).render(
      Xml.element(XmlElement.Pre).setChildren(Seq(Xml.text("a"), Xml.text("b")))
    )
    assert(dumped.contains(">ab<"), dumped)
    assert(!dumped.contains("a\nb"), dumped)
  }

  test("a constructed feed child stays in the parent default namespace") {
    val atom: String = "http://www.w3.org/2005/Atom"
    val feed: Xml.Element = Xml.element("feed")
      .set(XmlAttribute.Xmlns, atom)
      .setChildren(Seq(Xml.element("entry")))
    val dumped: String = render(feed)
    assert(dumped.contains(s"""xmlns="$atom""""), dumped)
    assert(!dumped.contains("xmlns=\"\""), dumped)
    assert(dumped.contains("<entry>"), dumped)
  }

  test("a constructed sitemap url stays in the parent default namespace") {
    val sitemapNs: String = "http://www.sitemaps.org/schemas/sitemap/0.9"
    val urlset: Xml.Element = Xml.element("urlset")
      .set(XmlAttribute.Xmlns, sitemapNs)
      .setChildren(Seq(Xml.element("url", Seq.empty, Seq(Xml.element("loc").setText("http://example.test/")))))
    val dumped: String = render(urlset)
    assert(!dumped.contains("xmlns=\"\""), dumped)
    assert(dumped.contains("<url>"), dumped)
    assert(dumped.contains("<loc>"), dumped)
  }

  test("plain CDATA in script stays a CDATA section") {
    val dumped: String = render(
      Xml.element(XmlElement.Script.qName, Seq.empty, Seq(Xml.cdata("a < b")))
    )
    assert(dumped.contains("<![CDATA[a < b]]>"), dumped)
  }

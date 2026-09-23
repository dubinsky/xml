package org.podval.xml

import ZioBlocksHtml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk
import zio.blocks.html.{Js, script, `type`}

final class HtmlXmlWriterConfigSpec extends AnyFunSuite:
  private def render(element: Xml.Element, width: Int = 40): String =
    HtmlXmlWriterConfig.render(element, width)

  private def renderHtml(element: ZioBlocksHtml.Element): String =
    HtmlXmlWriterConfig.render(element)

  test("span with two element children is not indented (no HTML space inside)") {
    val ref: Xml.Element = Xml
      .element(XmlElement.Span)
      .addClass("glossary-ref")
      .setChildren(Chunk(
        Xml.element(XmlElement.A).setHref("#posuk").setText("posuk"),
        Xml.element(XmlElement.Span).addClass("glossary-tip").setText("verse")
      ))
    val paragraph: Xml.Element = Xml
      .element(XmlElement.P)
      .setChildren(Chunk(Xml.text("("), ref, Xml.text(" 1)")))
    val rendered: String = render(paragraph)
    val inner: String = rendered.drop(rendered.indexOf("glossary-ref"))
    assert(rendered.contains("(<span"))
    assert(!rendered.contains("( <"))
    assert(inner.contains("</a><span"))
    assert(""">\s+<a""".r.findFirstIn(inner).isEmpty)
  }

  test("void elements self-close; empty non-void elements do not") {
    assert(render(Xml.element(XmlElement.Br)).contains("<br/>"))
    assert(render(Xml.element(XmlElement.Img).set(XmlAttribute.Src, "x")).contains("/>"))
    val script: String = render(Xml.element("script"))
    assert(script.contains("<script>"))
    assert(script.contains("</script>"))
    assert(!script.contains("<script/>"))
  }

  test("void elements self-close by local name") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<tei:br xmlns:tei="http://www.tei-c.org/ns/1.0"/>"""
    ).toOption.get
    val dumped: String = render(xml)
    assert(dumped.contains("<tei:br"), dumped)
    assert(dumped.contains("/>"), dumped)
    assert(!dumped.contains("</tei:br>"), dumped)
  }

  test("br with following whitespace forces a newline") {
    val xml: Xml.Element = XmlParser.parseXml("<p>foo<br/>\nbar</p>").toOption.get
    val dumped: String = render(xml, width = 120)
    assert(dumped.contains("<br/>\n"), dumped)
    assert(!dumped.contains("<br/> bar"), dumped)
  }

  test("br without following whitespace stays inline") {
    val xml: Xml.Element = XmlParser.parseXml("<p>foo<br/>bar</p>").toOption.get
    val dumped: String = render(xml, width = 120)
    assert(dumped.contains("foo<br/>bar"), dumped)
  }

  test("span still preserves a real space before an inner element") {
    val span: Xml.Element = Xml
      .element(XmlElement.Span)
      .setChildren(Chunk(Xml.text("foo "), Xml.element(XmlElement.Em).setText("bar")))
    val rendered: String = render(span)
    assert(rendered.contains("foo <em>bar</em>"))
  }

  test("adjacent phrasing children are not stacked") {
    val dumped: String = render(
      Xml.element(XmlElement.P).setChildren(Chunk(
        Xml.element(XmlElement.Em).setText("a"),
        Xml.element("strong").setText("b")
      )),
      width = 120
    )
    assert(dumped.contains("<em>a</em><strong>b</strong>"), dumped)
  }

  test("div with two paragraphs still stacks") {
    val dumped: String = render(
      Xml.element(XmlElement.Div).setChildren(Chunk(
        Xml.element(XmlElement.P).setText("a"),
        Xml.element(XmlElement.P).setText("b")
      )),
      width = 120
    )
    assert(dumped.contains("<div>\n"), dumped)
    assert(dumped.contains("</p>\n"), dumped)
  }

  test("script text keeps < and &&") {
    val dumped: String = render(Xml.element(XmlElement.Script).setText("if (a < b && c) {}"))
    assert(dumped.contains("if (a < b && c) {}"), dumped)
    assert(!dumped.contains("&lt;"), dumped)
    assert(!dumped.contains("&amp;"), dumped)
  }

  test("script newlines and // comments survive wrapping") {
    val dumped: String = render(
      Xml.element(XmlElement.Script).setText("a();\n// comment\nb();")
    )
    assert(dumped.contains("a();\n// comment\nb();"), dumped)
    assert(!dumped.contains("// comment b();"), dumped)
  }

  test("long script string with spaces is not wrapped") {
    val json: String = """{"name": "A Very Long Site Title With Spaces"}"""
    val dumped: String = render(
      Xml.element(XmlElement.Script).set("type", "application/ld+json").setText(json)
    )
    assert(dumped.contains(json), dumped)
    assert(!dumped.contains("A Very\n"), dumped)
  }

  test("script source </script> is broken") {
    val dumped: String = render(Xml.element(XmlElement.Script).setText("a</script>b"))
    assert(dumped.contains("""a<\/script>b"""), dumped)
    assert(dumped.contains("</script>"), dumped)
    assert(!dumped.contains("a</script>b"), dumped)
  }

  test("script source </SCRIPT> is broken") {
    val dumped: String = render(Xml.element(XmlElement.Script).setText("a</SCRIPT>b"))
    assert(dumped.contains("""a<\/SCRIPT>b"""), dumped)
  }

  test("inlineJs already broken </ is not double-escaped") {
    val dumped: String = renderHtml(script().inlineJs(Js("a</script>b")))
    assert(dumped.contains("""a<\/script>b"""), dumped)
    assert(!dumped.contains("""a<\\/script>b"""), dumped)
  }

  test("script(Js) without inlineJs still breaks </") {
    val dumped: String = renderHtml(script(Js("a</script>b")))
    assert(dumped.contains("""a<\/script>b"""), dumped)
  }

  test("style body </style> is broken") {
    val dumped: String = render(Xml.element(XmlElement.Style).setText("a</style>b"))
    assert(dumped.contains("""a<\/style>b"""), dumped)
    assert(!dumped.contains("a</style>b"), dumped)
  }

  test("style keeps > and <") {
    val dumped: String = render(Xml.element(XmlElement.Style).setText("""a > b; content:"<""""))
    assert(dumped.contains("""a > b; content:"<""""), dumped)
    assert(!dumped.contains("&lt;"), dumped)
  }

  test("module and json-ld scripts use the same raw rules") {
    val module: String = renderHtml(
      script(`type` := "module").inlineJs(Js("if (a < b) {}"))
    )
    assert(module.contains("if (a < b) {}"), module)
    val jsonLd: String = render(
      Xml.element(XmlElement.Script).set("type", "application/ld+json").setText("a < b")
    )
    assert(jsonLd.contains("a < b"), jsonLd)
  }

  test("pre still encodes <") {
    val dumped: String = render(Xml.element(XmlElement.Pre).setText("a < b"))
    assert(dumped.contains("a &lt; b"), dumped)
  }

  test("root pre keeps an embedded newline") {
    val dumped: String = render(Xml.element(XmlElement.Pre).setText("a\nb"))
    assert(dumped.contains("a\nb"), dumped)
    val inner: String = dumped.substring(dumped.indexOf('>') + 1, dumped.lastIndexOf('<'))
    assert(inner == "a\nb", inner)
  }

  test("script inside pre is raw") {
    val dumped: String = render(
      Xml.element(XmlElement.Pre).setChildren(Chunk(
        Xml.text("a < b"),
        Xml.element(XmlElement.Script).setText("c < d"),
        Xml.text("e < f")
      ))
    )
    assert(dumped.contains("a &lt; b"), dumped)
    assert(dumped.contains("<script>c < d</script>"), dumped)
    assert(dumped.contains("e &lt; f"), dumped)
  }

  test("nested script child end tag is broken; outer end tag is real") {
    val dumped: String = render(
      Xml.element(XmlElement.Script).setChildren(Seq(
        Xml.element(XmlElement.Script).setText("x")
      ))
    )
    assert(dumped.contains("""<script><script>x<\/script></script>"""), dumped)
  }

  test("pre inside script is serialized as tags and not encoded") {
    val dumped: String = render(
      Xml.element(XmlElement.Script).setChildren(Seq(
        Xml.element(XmlElement.Pre).setText("a < b")
      ))
    )
    assert(dumped.contains("<pre>a < b"), dumped)
    assert(dumped.contains("""<\/pre>"""), dumped)
    assert(!dumped.contains("&lt;"), dumped)
  }

  test("adjacent script text children concatenate") {
    val xml: String = render(
      Xml.element(XmlElement.Script).setChildren(Seq(Xml.text("a"), Xml.text("b")))
    )
    assert(xml.contains(">ab<"), xml)
    assert(!xml.contains("a\nb"), xml)
    val html: String = renderHtml(script(Js("a"), Js("b")))
    assert(html.contains(">ab<"), html)
    assert(!html.contains("a\nb"), html)
  }

  test("CDATA child of script is raw text") {
    val dumped: String = render(
      Xml.element(XmlElement.Script.qName, Seq.empty, Seq(Xml.cdata("a < b")))
    )
    assert(dumped.contains("a < b"), dumped)
    assert(!dumped.contains("<![CDATA["), dumped)
  }

  test("onclick still encodes <") {
    val dumped: String = render(Xml.element(XmlElement.Div).set("onclick", "a < b"))
    assert(dumped.contains("a &lt; b"), dumped)
  }

  test("title and textarea bodies still encode <") {
    val title: String = render(Xml.element(XmlElement.Title).setText("a < b"))
    assert(title.contains("a &lt; b"), title)
    val textarea: String = render(Xml.element("textarea").setText("a < b"))
    assert(textarea.contains("a &lt; b"), textarea)
  }

  test("empty script keeps an end tag") {
    val dumped: String = render(Xml.element(XmlElement.Script))
    assert(dumped.contains("<script></script>"), dumped)
    assert(!dumped.contains("<script/>"), dumped)
  }

  test("root script is not mixed-tokenized") {
    val dumped: String = render(
      Xml.element(XmlElement.Script).setText("foo bar baz quux"),
      width = 8
    )
    assert(dumped.contains(">foo bar baz quux<"), dumped)
  }

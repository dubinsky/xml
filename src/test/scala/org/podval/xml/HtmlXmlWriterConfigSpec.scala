package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class HtmlXmlWriterConfigSpec extends AnyFunSuite:
  private def render(element: Xml.Element, width: Int = 40): String =
    HtmlXmlWriterConfig.render(element, width)

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
    assert(!""">\s+<a""".r.findFirstIn(inner).isDefined)
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

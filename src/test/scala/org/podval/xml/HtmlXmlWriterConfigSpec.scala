package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class HtmlXmlWriterConfigSpec extends AnyFunSuite:
  private def render(element: Xml.Element, width: Int = 40): String =
    HtmlXmlWriterConfig.render(element, width)

  test("span with two element children is not indented (no HTML space inside)") {
    val ref: Xml.Element = Xml
      .element("span")
      .addClass("glossary-ref")
      .setChildren(Chunk(
        Xml.element("a").setHref("#posuk").setText("posuk"),
        Xml.element("span").addClass("glossary-tip").setText("verse")
      ))
    val paragraph: Xml.Element = Xml
      .element("p")
      .setChildren(Chunk(Xml.text("("), ref, Xml.text(" 1)")))
    val rendered: String = render(paragraph)
    val inner: String = rendered.drop(rendered.indexOf("glossary-ref"))
    assert(rendered.contains("(<span"))
    assert(!rendered.contains("( <"))
    assert(inner.contains("</a><span"))
    assert(!""">\s+<a""".r.findFirstIn(inner).isDefined)
  }

  test("void elements self-close; empty non-void elements do not") {
    assert(render(Xml.element("br")).contains("<br/>"))
    assert(render(Xml.element("img").set("src", "x")).contains("/>"))
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

  test("span still preserves a real space before an inner element") {
    val span: Xml.Element = Xml
      .element("span")
      .setChildren(Chunk(Xml.text("foo "), Xml.element("em").setText("bar")))
    val rendered: String = render(span)
    assert(rendered.contains("foo <em>bar</em>"))
  }

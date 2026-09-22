package org.podval.xml

import ZioBlocksXml.given
import ZioBlocksHtml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class XmlAstSpec extends AnyFunSuite:
  private def parse(input: String): ZioBlocksXml.Element =
    XmlParser.parseXml(input).toOption.get

  test("convertElements keeps mixed text and elements") {
    val converted: ZioBlocksXml.Nodes = parse("<p>a<x/>b</p>").getChildren.convertElements(_ => None)
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.getName.qName) == Seq("x"))
  }

  test("convertElements can expand an element among text") {
    val converted: ZioBlocksXml.Nodes = parse("<p>a<note/>b</p>").getChildren.convertElements(
      el => Option.when(el.isNamed("note"))(Chunk(ZioBlocksXml.element(XmlElement.Span), ZioBlocksXml.element("aside")))
    )
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.getName.qName) == Seq("span", "aside"))
  }

  test("to[ZioBlocksHtml.Element] keeps mixed text and elements") {
    val dumped: String = HtmlXmlWriterConfig.render(parse("<p>a<x/>b</p>").to[ZioBlocksHtml.Element])
    assert(dumped.contains("a"), dumped)
    assert(dumped.contains("b"), dumped)
    assert(dumped.contains("<x"), dumped)
  }

  test("to[ZioBlocksHtml.Element] keeps attributes and nesting") {
    val html: ZioBlocksHtml.Element = parse("""<div xml:id="x" class="y"><p>a</p></div>""").to[ZioBlocksHtml.Element]
    assert(html.isElement(XmlElement.Div))
    assert(html.get(XmlAttribute.XmlId).contains("x"))
    assert(html.get(XmlAttribute.CssClass).contains("y"))
    assert(html.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("p"))
  }

  test("to[ZioBlocksHtml.Element] keeps empty elements") {
    val html: ZioBlocksHtml.Element = parse("<x/>").to[ZioBlocksHtml.Element]
    assert(html.isNamed("x"))
    assert(html.getChildren.isEmpty)
  }

  test("to[ZioBlocksHtml.Element] drops comments and processing instructions") {
    val html: ZioBlocksHtml.Element = parse("<p>a<!--c--><?pi d?>b</p>").to[ZioBlocksHtml.Element]
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a", "b"))
    assert(html.getChildren.flatMap(_.asElement).isEmpty)
  }

  test("to[ZioBlocksHtml.Element] turns CDATA into HTML text encoded on write") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(XmlElement.P.qName, Seq.empty, Seq(ZioBlocksXml.cdata("a<b")))
    val html: ZioBlocksHtml.Element = xml.to[ZioBlocksHtml.Element]
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a<b"))
    val dumped: String = HtmlXmlWriterConfig.render(html)
    assert(dumped.contains("a&lt;b"), dumped)
    assert(!dumped.contains("a<b"), dumped)
  }

  test("toId trims and replaces spaces") {
    assert(XmlAst.toId("  a b  ") == "a-b")
  }

  test("elementById finds a descendant") {
    val xml: ZioBlocksXml.Element = parse("""<div><p id="n1">a</p></div>""")
    assert(xml.elementById("n1").getText == "a")
  }

  test("requireName and childrenNamed") {
    val xml: ZioBlocksXml.Element = parse("""<torah><aliyah n="1"/><aliyah n="2"/></torah>""")
    xml.requireName("torah")
    xml.requireNoOther(Set("aliyah"))
    assert(xml.childrenNamed("aliyah").map(_.requireAttr("n")) == Seq("1", "2"))
    assert(xml.childrenNamed("aliyah").head.positiveInt("n") == 1)
  }

  test("isInclude requires XInclude namespace or xi prefix") {
    val withNs: ZioBlocksXml.Element = parse(
      s"""<xi:include xmlns:xi="${XmlNamespace.xinclude.uri}" href="a.xml"/>"""
    )
    assert(withNs.isInclude)
    val prefixed: ZioBlocksXml.Element = ZioBlocksXml.element("xi:include", Seq(XmlAttribute.Href.qName -> "a.xml"), Seq.empty)
    assert(prefixed.isInclude)
    val bare: ZioBlocksXml.Element = parse("""<include href="a.xml"/>""")
    assert(!bare.isInclude)
    val noHref: ZioBlocksXml.Element = parse(
      s"""<xi:include xmlns:xi="${XmlNamespace.xinclude.uri}"/>"""
    )
    assert(!noHref.isInclude)
  }

  test("withAttribute matches xml:id by URI and local name") {
    val xml: ZioBlocksXml.Element = parse("""<p xml:id="old" n="1"/>""")
    val updated: ZioBlocksXml.Element = xml.set(XmlAttribute.XmlId, "new")
    assert(updated.get(XmlAttribute.XmlId).contains("new"))
    assert(updated.get("n").contains("1"))
    assert(updated.getAttributes.count((name, _) => name.localName == "id") == 1)
  }

  test("fold dispatches node kinds") {
    val xml: ZioBlocksXml.Element = parse("<p>a<!--c--><?pi d?><![CDATA[b]]><x/></p>")
    val kinds: Seq[String] = xml.getChildren.map: n =>
      n.fold(
        element = _ => "el",
        text = _ => "text",
        cdata = _ => "cdata",
        comment = _ => "comment",
        processingInstruction = (_, _) => "pi",
        unknown = "unknown"
      )
    assert(kinds == Seq("text", "comment", "pi", "cdata", "el"))
  }

  test("get(XmlAttribute) matches expanded names") {
    val xml: ZioBlocksXml.Element = parse("""<p xml:id="n1" id="n2" xml:lang="en" lang="fr"/>""")
    assert(xml.get(XmlAttribute.XmlId).contains("n1"))
    assert(xml.get(XmlAttribute.Id).contains("n2"))
    assert(xml.get(XmlAttribute.XmlLang).contains("en"))
    assert(xml.get(XmlAttribute.Lang).contains("fr"))
    assert(XmlAttribute.XmlId.qName == "xml:id")
    assert(XmlAttribute.XmlId.name.uri.contains(XmlNamespace.xml.uri))
    assert(XmlAttribute.XmlLang.qName == "xml:lang")
    assert(XmlAttribute.XmlBase.qName == "xml:base")
    assert(XmlAttribute.Xmlns.qName == "xmlns")
    assert(XmlAttribute.Xmlns.name.prefix.isEmpty)
    assert(XmlAttribute.Xmlns.name.uri.contains(XmlNamespace.xmlns.uri))
    assert(XmlAttribute.Xmlns("xsi").qName == "xmlns:xsi")
    assert(XmlAttribute.Lang.qName == "lang")
    assert(!xml.isElement(XmlElement.A))
    assert(parse("<a/>").isA)
    assert(parse("""<a xmlns="http://docbook.org/ns/docbook"/>""").isA)
    assert(!parse("""<tei:a xmlns:tei="http://www.tei-c.org/ns/1.0"/>""").isA)
  }

  test("CssClass is a token; HtmlClass is the class attribute") {
    val xml: ZioBlocksXml.Element = ZioBlocksXml.element(XmlElement.P).add(CssClass("x")).add(CssClass("x"))
    assert(xml.has(CssClass("x")))
    assert(!xml.has(CssClass("y")))
    assert(xml.get(XmlAttribute.CssClass).contains("x"))
    assert(xml.getClasses == Seq("x"))
  }

  test("renameKeepingClass stamps the old local name as a class") {
    val xml: ZioBlocksXml.Element = parse("""<tei:p xmlns:tei="http://www.tei-c.org/ns/1.0">a</tei:p>""")
    val renamed: ZioBlocksXml.Element = xml.renameKeepingClass("tei-p")
    assert(renamed.getName.qName == "tei-p")
    assert(renamed.getClasses.contains("p"))
    assert(xml.rename("div").getClasses.isEmpty)
  }

  test("transform stopAtCode matches local name") {
    val xml: ZioBlocksXml.Element = parse(
      """<tei:code xmlns:tei="http://www.tei-c.org/ns/1.0"><x/></tei:code>"""
    )
    val transformed: ZioBlocksXml.Element = xml.transform(_.rename("y"))
    assert(transformed.getName.qName == "tei:code")
    assert(transformed.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("x"))
  }

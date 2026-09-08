package org.podval.xml

import Html.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class XmlAstSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  test("convertElements keeps mixed text and elements") {
    val converted: Xml.Nodes = parse("<p>a<x/>b</p>").getChildren.convertElements(_ => None)
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.qName) == Seq("x"))
  }

  test("convertElements can expand an element among text") {
    val converted: Xml.Nodes = parse("<p>a<note/>b</p>").getChildren.convertElements(
      el => Option.when(el.qName == "note")(Chunk(Xml.element(XmlElement.Span), Xml.element("aside")))
    )
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.qName) == Seq("span", "aside"))
  }

  test("to[Html.Element] keeps mixed text and elements") {
    val dumped: String = HtmlXmlWriterConfig.render(parse("<p>a<x/>b</p>").to[Html.Element])
    assert(dumped.contains("a"), dumped)
    assert(dumped.contains("b"), dumped)
    assert(dumped.contains("<x"), dumped)
  }

  test("to[Html.Element] keeps attributes and nesting") {
    val html: Html.Element = parse("""<div xml:id="x" class="y"><p>a</p></div>""").to[Html.Element]
    assert(html.isElement(XmlElement.Div))
    assert(html.get(XmlAttribute.XmlId).contains("x"))
    assert(html.get(XmlAttribute.CssClass).contains("y"))
    assert(html.getChildren.flatMap(_.asElement).map(_.qName) == Seq("p"))
  }

  test("to[Html.Element] keeps empty elements") {
    val html: Html.Element = parse("<x/>").to[Html.Element]
    assert(html.qName == "x")
    assert(html.getChildren.isEmpty)
  }

  test("to[Html.Element] drops comments and processing instructions") {
    val html: Html.Element = parse("<p>a<!--c--><?pi d?>b</p>").to[Html.Element]
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a", "b"))
    assert(html.getChildren.flatMap(_.asElement).isEmpty)
  }

  test("to[Html.Element] turns CDATA into HTML text encoded on write") {
    val xml: Xml.Element = Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("a<b")))
    val html: Html.Element = xml.to[Html.Element]
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a<b"))
    val dumped: String = HtmlXmlWriterConfig.render(html)
    assert(dumped.contains("a&lt;b"), dumped)
    assert(!dumped.contains("a<b"), dumped)
  }

  test("toId trims and replaces spaces") {
    assert(XmlAst.toId("  a b  ") == "a-b")
  }

  test("elementById finds a descendant") {
    val xml: Xml.Element = parse("""<div><p id="n1">a</p></div>""")
    assert(xml.elementById("n1").getText == "a")
  }

  test("requireName and childrenNamed") {
    val xml: Xml.Element = parse("""<torah><aliyah n="1"/><aliyah n="2"/></torah>""")
    xml.requireName("torah")
    xml.requireNoOther(Set("aliyah"))
    assert(xml.childrenNamed("aliyah").map(_.requireAttr("n")) == Seq("1", "2"))
    assert(xml.childrenNamed("aliyah").head.positiveInt("n") == 1)
  }

  test("isInclude requires XInclude namespace or xi prefix") {
    val withNs: Xml.Element = parse(
      s"""<xi:include xmlns:xi="${XmlNamespace.xinclude.uri}" href="a.xml"/>"""
    )
    assert(withNs.isInclude)
    val prefixed: Xml.Element = Xml.element("xi:include", Seq(XmlAttribute.Href.qName -> "a.xml"), Seq.empty)
    assert(prefixed.isInclude)
    val bare: Xml.Element = parse("""<include href="a.xml"/>""")
    assert(!bare.isInclude)
    val noHref: Xml.Element = parse(
      s"""<xi:include xmlns:xi="${XmlNamespace.xinclude.uri}"/>"""
    )
    assert(!noHref.isInclude)
  }

  test("withAttribute matches xml:id by URI and local name") {
    val xml: Xml.Element = parse("""<p xml:id="old" n="1"/>""")
    val updated: Xml.Element = xml.set(XmlAttribute.XmlId, "new")
    assert(updated.get(XmlAttribute.XmlId).contains("new"))
    assert(updated.get("n").contains("1"))
    assert(updated.getExpandedAttributes.count((name, _) => name.localName == "id") == 1)
  }

  test("fold dispatches node kinds") {
    val xml: Xml.Element = parse("<p>a<!--c--><?pi d?><![CDATA[b]]><x/></p>")
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
    val xml: Xml.Element = parse("""<p xml:id="n1" id="n2" xml:lang="en" lang="fr"/>""")
    assert(xml.get(XmlAttribute.XmlId).contains("n1"))
    assert(xml.get(XmlAttribute.Id).contains("n2"))
    assert(xml.get(XmlAttribute.XmlLang).contains("en"))
    assert(xml.get(XmlAttribute.Lang).contains("fr"))
    assert(XmlAttribute.XmlId.qName == "xml:id")
    assert(XmlAttribute.XmlId.expanded.uri.contains(XmlNamespace.xml.uri))
    assert(XmlAttribute.XmlLang.qName == "xml:lang")
    assert(XmlAttribute.XmlBase.qName == "xml:base")
    assert(XmlAttribute.Xmlns.qName == "xmlns")
    assert(XmlAttribute.Xmlns.expanded.prefix.isEmpty)
    assert(XmlAttribute.Xmlns.expanded.uri.contains(XmlNamespace.xmlns.uri))
    assert(XmlAttribute.Xmlns("xsi").qName == "xmlns:xsi")
    assert(XmlAttribute.Lang.qName == "lang")
    assert(!xml.isElement(XmlElement.A))
    assert(parse("<a/>").isA)
    assert(parse("""<a xmlns="http://docbook.org/ns/docbook"/>""").isA)
    assert(!parse("""<tei:a xmlns:tei="http://www.tei-c.org/ns/1.0"/>""").isA)
  }

  test("CssClass is a token; HtmlClass is the class attribute") {
    val xml: Xml.Element = Xml.element(XmlElement.P).add(CssClass("x")).add(CssClass("x"))
    assert(xml.has(CssClass("x")))
    assert(!xml.has(CssClass("y")))
    assert(xml.get(XmlAttribute.CssClass).contains("x"))
    assert(xml.getClasses == Seq("x"))
  }

  test("renameKeepingClass stamps the old local name as a class") {
    val xml: Xml.Element = parse("""<tei:p xmlns:tei="http://www.tei-c.org/ns/1.0">a</tei:p>""")
    val renamed: Xml.Element = xml.renameKeepingClass("tei-p")
    assert(renamed.qName == "tei-p")
    assert(renamed.getClasses.contains("p"))
    assert(xml.rename("div").getClasses.isEmpty)
  }

  test("transform stopAtCode matches local name") {
    val xml: Xml.Element = parse(
      """<tei:code xmlns:tei="http://www.tei-c.org/ns/1.0"><x/></tei:code>"""
    )
    val transformed: Xml.Element = xml.transform(_.rename("y"))
    assert(transformed.qName == "tei:code")
    assert(transformed.getChildren.flatMap(_.asElement).map(_.qName) == Seq("x"))
  }

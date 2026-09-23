package org.podval.xml

import XmlNode.convertElements
import ZioBlocksHtml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk

final class XmlAstSpec extends AnyFunSuite:
  private def parse(input: String): Xml.Element =
    XmlParser.parseXml(input).toOption.get

  test("convertElements keeps mixed text and elements") {
    val converted: Xml.Nodes = parse("<p>a<x/>b</p>").getChildren.convertElements(_ => None)
    assert(converted.flatMap(_.asText) == Seq("a", "b"))
    assert(converted.flatMap(_.asElement).map(_.getName.qName) == Seq("x"))
  }

  test("convertElements can expand an element among text") {
    val converted: Xml.Nodes = parse("<p>a<note/>b</p>").getChildren.convertElements(
      el => Option.when(el.isNamed("note"))(Chunk(Xml.element(XmlElement.Span), Xml.element("aside")))
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
    assert(html.getName.is(XmlElement.Div))
    assert(html.getAttributes.collectFirst { case (n, v) if n.is(XmlAttribute.XmlId) => v }.contains("x"))
    assert(html.getAttributes.collectFirst { case (n, v) if n.is(XmlAttribute.CssClass) => v }.contains("y"))
    assert(html.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("p"))
  }

  test("to[ZioBlocksHtml.Element] keeps empty elements") {
    val html: ZioBlocksHtml.Element = parse("<x/>").to[ZioBlocksHtml.Element]
    assert(html.getName.matches("x"))
    assert(html.getChildren.isEmpty)
  }

  test("to[ZioBlocksHtml.Element] drops comments and processing instructions") {
    val html: ZioBlocksHtml.Element = parse("<p>a<!--c--><?pi d?>b</p>").to[ZioBlocksHtml.Element]
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a", "b"))
    assert(html.getChildren.flatMap(_.asElement).isEmpty)
  }

  test("to[ZioBlocksHtml.Element] turns CDATA into HTML text encoded on write") {
    val xml: Xml.Element = Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("a<b")))
    val html: ZioBlocksHtml.Element = xml.to[ZioBlocksHtml.Element]
    assert(html.getChildren.flatMap(_.asAtom) == Seq("a<b"))
    val dumped: String = HtmlXmlWriterConfig.render(html)
    assert(dumped.contains("a&lt;b"), dumped)
    assert(!dumped.contains("a<b"), dumped)
  }

  test("toId trims and replaces spaces") {
    assert(XmlAst.toId("  a b  ") == "a-b")
  }

  test("getById finds a descendant") {
    val xml: Xml.Element = parse("""<div><p id="n1">a</p></div>""")
    assert(xml.getById("n1").map(_.getText).contains("a"))
    assert(xml.getById("missing").isEmpty)
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
    assert(updated.getAttributes.count((name, _) => name.localName == "id") == 1)
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
    val xml: Xml.Element = Xml.element(XmlElement.P).add(CssClass("x")).add(CssClass("x"))
    assert(xml.has(CssClass("x")))
    assert(!xml.has(CssClass("y")))
    assert(xml.get(XmlAttribute.CssClass).contains("x"))
    assert(xml.getClasses == Seq("x"))
  }

  test("renameKeepingClass stamps the old local name as a class") {
    val xml: Xml.Element = parse("""<tei:p xmlns:tei="http://www.tei-c.org/ns/1.0">a</tei:p>""")
    val renamed: Xml.Element = xml.renameKeepingClass("tei-p")
    assert(renamed.getName.qName == "tei-p")
    assert(renamed.getClasses.contains("p"))
    assert(xml.rename("div").getClasses.isEmpty)
  }

  test("transform stopAtCode matches local name") {
    val xml: Xml.Element = parse(
      """<tei:code xmlns:tei="http://www.tei-c.org/ns/1.0"><x/></tei:code>"""
    )
    val transformed: Xml.Element = xml.transform(_.rename("y"))
    assert(transformed.getName.qName == "tei:code")
    assert(transformed.childElements.map(_.getName.qName) == Seq("x"))
  }

  test("rewrite Keep with default stopAtCode does not rename inside tei:code") {
    val xml: Xml.Element = parse(
      """<div><tei:code xmlns:tei="http://www.tei-c.org/ns/1.0"><x/></tei:code></div>"""
    )
    val rewritten: Xml.Element = xml.rewrite((el, _) => Xml.Rewrite.Keep(el.rename("y")))
    assert(rewritten.getName.qName == "y")
    val code: Xml.Element = rewritten.childElements.head
    assert(code.getName.qName == "tei:code")
    assert(code.childElements.map(_.getName.qName) == Seq("x"))
  }

  test("rewrite Emit does not visit the nodes it inserts") {
    val xml: Xml.Element = parse("<p><note/></p>")
    var seen: Seq[String] = Seq.empty
    val rewritten: Xml.Element = xml.rewrite: (el, _) =>
      seen = seen :+ el.getName.qName
      if el.isNamed("note") then
        Xml.Rewrite.Emit(Seq(Xml.element("span", Seq.empty, Seq(Xml.element("inner")))))
      else
        Xml.Rewrite.Keep(el)
    assert(seen == Seq("p", "note"))
    val span: Xml.Element = rewritten.childElements.head
    assert(span.getName.qName == "span")
    assert(span.childElements.map(_.getName.qName) == Seq("inner"))
  }

  test("elements collects elements that match") {
    val xml: Xml.Element = parse("<div><p id=\"a\"/><span/><p id=\"b\"/></div>")
    assert(xml.elements(_.isNamed("p")).flatMap(_.getId) == Seq("a", "b"))
    assert(xml.elements(_.isNamed("span"), stopAtCode = false).map(_.getName.qName) == Seq("span"))
  }

  test("rewrite Replace of a child runs on a nested element inside the replacement") {
    val xml: Xml.Element = parse("<p><note/></p>")
    var seen: Seq[String] = Seq.empty
    val rewritten: Xml.Element = xml.rewrite: (el, _) =>
      seen = seen :+ el.getName.qName
      if el.isNamed("note") then
        Xml.Rewrite.Replace(Seq(Xml.element("span", Seq.empty, Seq(Xml.element("inner")))))
      else if el.isNamed("inner") then
        Xml.Rewrite.Keep(el.rename("done"))
      else
        Xml.Rewrite.Keep(el)
    assert(seen == Seq("p", "note", "span", "inner"))
    val span: Xml.Element = rewritten.childElements.head
    assert(span.getName.qName == "span")
    assert(span.childElements.map(_.getName.qName) == Seq("done"))
  }

  test("rewrite does not double-wrap code when the parent is pre") {
    def wrap(el: Xml.Element, parent: Option[Xml.Element]): Xml.Rewrite =
      if el.isNamed("code") && !parent.exists(_.isNamed("pre")) then
        Xml.Rewrite.Replace(Seq(Xml.element("pre", Seq.empty, Seq(el))))
      else
        Xml.Rewrite.Keep(el)

    val wrapped: Xml.Element = parse("<div><code>x</code></div>").rewrite(wrap, stopAtCode = false)
    val pre: Xml.Element = wrapped.childElements.head
    assert(pre.getName.qName == "pre")
    assert(pre.childElements.map(_.getName.qName) == Seq("code"))
    assert(pre.childElements.head.getText == "x")

    val already: Xml.Element = parse("<pre><code>x</code></pre>").rewrite(wrap, stopAtCode = false)
    assert(already.getName.qName == "pre")
    assert(already.childElements.map(_.getName.qName) == Seq("code"))
    assert(already.childElements.head.getText == "x")
  }

  test("rewrite throws when a root Replace is two nodes") {
    val xml: Xml.Element = parse("<p/>")
    val error: XmlError = intercept[XmlError] {
      xml.rewrite: (el, _) =>
        if el.isNamed("p") then Xml.Rewrite.Replace(Seq(Xml.element("a"), Xml.element("b")))
        else Xml.Rewrite.Keep(el)
    }
    assert(error.getMessage == "rewrite must leave exactly one element")
  }

  test("omitting stopAtCode stops rewrite at code") {
    val xml: Xml.Element = parse("<div><code><x/></code></div>")
    val rewritten: Xml.Element = xml.rewrite((el, _) => Xml.Rewrite.Keep(el.rename("y")))
    assert(rewritten.getName.qName == "y")
    val code: Xml.Element = rewritten.childElements.head
    assert(code.getName.qName == "code")
    assert(code.childElements.map(_.getName.qName) == Seq("x"))
  }

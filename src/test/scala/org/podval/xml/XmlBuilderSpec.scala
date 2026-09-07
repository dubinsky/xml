package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class XmlBuilderSpec extends AnyFunSuite:
  private def parse[E: XmlAst](build: XmlBuilder[E] => Unit): E =
    val builder: XmlBuilder[E] = XmlBuilder()
    builder.startElement(summon[XmlAst[E]].element("p"))
    build(builder)
    builder.endElement()
    builder.result

  test("consecutive text chunks merge") {
    val xml: Xml.Element = parse: b =>
      b.text("a")
      b.text("b")
    assert(xml.getChildren.flatMap(_.asText) == Seq("ab"))
    assert(xml.getChildren.flatMap(_.asCData).isEmpty)
  }

  test("text and CDATA stay distinct") {
    val xml: Xml.Element = parse: b =>
      b.text("a")
      b.cdata("b")
      b.text("c")
    assert(xml.getChildren.flatMap(_.asText) == Seq("a", "c"))
    assert(xml.getChildren.flatMap(_.asCData) == Seq("b"))
  }

  test("adjacent CDATA sections stay distinct") {
    val xml: Xml.Element = parse: b =>
      b.cdata("a")
      b.cdata("b")
    assert(xml.getChildren.flatMap(_.asCData) == Seq("a", "b"))
  }

  test("empty text and CDATA are dropped") {
    val xml: Xml.Element = parse: b =>
      b.text("")
      b.cdata("")
      b.text("a")
    assert(xml.getChildren.flatMap(_.asText) == Seq("a"))
    assert(xml.getChildren.flatMap(_.asCData).isEmpty)
  }

  test("ScalaXml consecutive text chunks merge") {
    val xml: ScalaXml.Element = parse: b =>
      b.text("a")
      b.text("b")
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asText) == Seq("ab"))
  }

  test("HTML builder drops comments") {
    val html: Html.Element = parse: b =>
      b.text("a")
      b.comment("c")
      b.text("b")
    assert(html.getChildren.flatMap(_.asAtom) == Seq("ab"))
    assert(html.getChildren.flatMap(_.asComment).isEmpty)
  }

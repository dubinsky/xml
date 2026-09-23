package org.podval.xml

import Xml.given
import ZioBlocksHtml.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite

final class XmlBuilderSpec extends AnyFunSuite:
  private def parse[E: XmlAst](build: XmlBuilder[E] => Unit): E =
    val builder: XmlBuilder[E] = XmlBuilder()
    builder.startElement(summon[XmlAst[E]].element(XmlElement.P))
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
    val html: ZioBlocksHtml.Element = parse: b =>
      b.text("a")
      b.comment("c")
      b.text("b")
    assert(html.getChildren.flatMap(_.asAtom) == Seq("ab"))
    assert(html.getChildren.flatMap(_.asComment).isEmpty)
  }

  test("comments before and after the root go on the document") {
    val builder: XmlBuilder[Xml.Element] = XmlBuilder()
    builder.comment("before")
    builder.startElement(Xml.element(XmlElement.P))
    builder.comment("inside")
    builder.endElement()
    builder.comment("after")
    val doc: XmlDocument[Xml.Element] = builder.document
    assert(doc.prolog == Seq(XmlNode.Comment("before")))
    assert(doc.epilogue == Seq(XmlNode.Comment("after")))
    assert(doc.root.getChildren.flatMap(_.asComment) == Seq("inside"))
  }

  test("nested elements are built from the child buffer") {
    val builder: XmlBuilder[Xml.Element] = XmlBuilder()
    builder.startElement(Xml.element("outer"))
    builder.startElement(Xml.element("inner"))
    builder.text("a")
    builder.endElement()
    builder.endElement()
    val xml: Xml.Element = builder.result
    assert(xml.isNamed("outer"))
    assert(xml.childElements.map(_.getName.qName) == Seq("inner"))
    assert(xml.childElements.head.getChildren.flatMap(_.asText) == Seq("a"))
  }

  test("result requires a document element") {
    intercept[IllegalArgumentException](XmlBuilder[Xml.Element]().result)
  }

  test("HTML document keeps prologue comments that the tree drops") {
    val builder: XmlBuilder[ZioBlocksHtml.Element] = XmlBuilder()
    builder.comment("c")
    builder.startElement(ZioBlocksHtml.element(XmlElement.P))
    builder.endElement()
    assert(builder.document.prolog == Seq(XmlNode.Comment("c")))
    assert(builder.result.getChildren.flatMap(_.asComment).isEmpty)
  }

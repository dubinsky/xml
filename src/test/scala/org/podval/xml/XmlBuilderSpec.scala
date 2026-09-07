package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml as ZXml, XmlName}

final class XmlBuilderSpec extends AnyFunSuite:
  private def parse(build: XmlBuilder => Unit): Xml.Element =
    val builder: XmlBuilder = XmlBuilder()
    builder.startElement(ZXml.Element(
      name = XmlName("p"),
      children = Chunk.empty,
      attributes = Chunk.empty
    ))
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

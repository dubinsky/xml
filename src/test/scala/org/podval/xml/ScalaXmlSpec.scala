package org.podval.xml

import Xml.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite

final class ScalaXmlSpec extends AnyFunSuite:
  test("element uses the given name") {
    val el: ScalaXml.Element = ScalaXml.element(XmlElement.P)
    assert(el.getName.qName == "p")
    assert(el.label == "p")
    assert(el.prefix == null)
  }

  test("element takes attributes and children") {
    val el: ScalaXml.Element = ScalaXml.element(
      "p",
      Seq("xml:id" -> "x"),
      Seq(ScalaXml.text("a"))
    )
    assert(el.getName.qName == "p")
    assert(XmlName.asPairs(ScalaXml.getAttributes(el)) == Seq("xml:id" -> "x"))
    assert(ScalaXml.getChildren(el).flatMap(ScalaXml.asText) == Seq("a"))
  }

  test("qualified element names round-trip prefix and label") {
    val el: ScalaXml.Element = ScalaXml.element("tei:p")
    assert(el.getName.qName == "tei:p")
    assert(el.prefix == "tei")
    assert(el.label == "p")
    val renamed: ScalaXml.Element = ScalaXml.withName(el, "div")
    assert(renamed.getName.qName == "div")
    assert(renamed.prefix == null)
    assert(renamed.label == "div")
  }

  test("qName reconstructs prefix from an existing Elem") {
    val existing: scala.xml.Elem = scala.xml.Elem(
      "tei",
      "p",
      scala.xml.Null,
      scala.xml.TopScope,
      false
    )
    assert(existing.getName.qName == "tei:p")
  }

  test("attributes preserve order and prefixes") {
    val el: ScalaXml.Element = ScalaXml.element(
      "p",
      Seq("id" -> "a", "xml:id" -> "b", "class" -> "c"),
      Seq.empty
    )
    assert(XmlName.asPairs(ScalaXml.getAttributes(el)) == Seq("id" -> "a", "xml:id" -> "b", "class" -> "c"))
  }

  test("cdata is PCData") {
    val node: ScalaXml.Node = ScalaXml.cdata("a<b")
    assert(ScalaXml.asCData(node).contains("a<b"))
    assert(ScalaXml.asAtom(node).contains("a<b"))
    assert(ScalaXml.asText(node).isEmpty)
  }

  test("to round-trips Xml through ScalaXml") {
    val xml: Xml.Element = XmlParser.parseXml("""<p xml:id="x"><q>a</q>b</p>""").toOption.get
    val round: Xml.Element = ScalaXml.converted(xml.to[ScalaXml.Element])
    assert(round.isElement(XmlElement.P))
    assert(round.get(XmlAttribute.XmlId).contains("x"))
    assert(round.childElements.map(_.getName.qName) == Seq("q"))
    assert(round.getChildren.flatMap(_.asText) == Seq("b"))
  }

  test("to round-trips CDATA through ScalaXml") {
    val xml: Xml.Element = Xml.element(XmlElement.P.qName, Seq.empty, Seq(Xml.cdata("a<b")))
    val scalaXml: ScalaXml.Element = xml.to[ScalaXml.Element]
    assert(ScalaXml.getChildren(scalaXml).flatMap(ScalaXml.asCData) == Seq("a<b"))
    val round: Xml.Element = ScalaXml.converted(scalaXml)
    assert(round.getChildren.flatMap(_.asCData) == Seq("a<b"))
  }

  test("to round-trips comments and processing instructions through ScalaXml") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<!--c--><?pi d?>b</p>").toOption.get
    val round: Xml.Element = ScalaXml.converted(xml.to[ScalaXml.Element])
    assert(round.getChildren.flatMap(_.asText) == Seq("a", "b"))
    assert(round.getChildren.flatMap(_.asComment) == Seq("c"))
    assert(round.getChildren.flatMap(_.asProcessingInstruction) == Seq(("pi", "d")))
  }

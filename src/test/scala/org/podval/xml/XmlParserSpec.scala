package org.podval.xml

import ZioBlocksHtml.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite

final class XmlParserSpec extends AnyFunSuite:
  private def children(element: Xml.Element): Seq[Xml.Element] =
    element.childElements

  test("string parse does not expand xi:include") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<includer>
        |  <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="includee.xml"/>
        |</includer>""".stripMargin
    ).toOption.get
    assert(xml.isNamed("includer"))
    assert(children(xml).map(_.getName.localName) == Seq("include"))
  }

  test("resource parse does not expand xi:include") {
    val xml: Xml.Element = XmlParser.parseResource(classOf[XmlParserSpec], "includer.xml").toOption.get
    assert(children(xml).map(_.getName.localName) == Seq("include"))
    assert(children(xml).flatMap(_.get(XmlAttribute.Href)) == Seq("includee.xml"))
  }

  test("loadCatalog resource name and wrapper can differ") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val decoded: Seq[Language] = XmlParser.loadCatalog(this, "languages", codec, "Languages")
    assert(decoded.map(_.ident) == Seq("ru", "he"))
  }

  test("missing resource is Left") {
    val result: Either[XmlError, Xml.Element] =
      XmlParser.parseResource(classOf[XmlParserSpec], "no-such.xml")
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Resource not found"))
  }

  test("attemptCatalog is Left on a missing resource") {
    val codec: XmlCodec[Language] = XmlCodec.derived(using Language.schema)
    val result: Either[XmlError, Seq[Language]] = XmlParser.attemptCatalog(this, "no-such", codec)
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Resource not found"))
  }

  test("parseXml does not attach comments outside the root element") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<?xml version="1.0"?>
        |<!-- prologue -->
        |<Day><names/></Day>""".stripMargin
    ).toOption.get
    assert(xml.isNamed("Day"))
    assert(children(xml).map(_.getName.qName) == Seq("names"))
  }

  test("parseHtml parses a fragment") {
    val xml: Xml.Element = XmlParser.parseHtml("<p>a<b>c</b></p>").toOption.get
    assert(xml.isElement(XmlElement.P))
    assert(children(xml).map(_.getName.qName) == Seq("b"))
    assert(xml.getText == "ac")
  }

  test("keeps text on both sides of a comment") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<!--c-->b</p>").toOption.get
    assert(xml.getChildren.flatMap(_.asText) == Seq("a", "b"))
    assert(xml.getChildren.flatMap(_.asComment) == Seq("c"))
    assert(xml.childElements.isEmpty)
  }

  test("keeps a processing instruction as a child") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<?pi d?>b</p>").toOption.get
    assert(xml.getChildren.flatMap(_.asText) == Seq("a", "b"))
    assert(xml.getChildren.flatMap(_.asProcessingInstruction) == Seq(("pi", "d")))
  }

  test("parseXml into ScalaXml keeps names, text, CDATA, and comments") {
    val xml: ScalaXml.Element = XmlParser.parseXml[ScalaXml.Element](
      """<p xml:id="x">a<![CDATA[b]]><!--c--></p>"""
    ).toOption.get
    assert(xml.getName.qName == "p")
    assert(ScalaXml.getAttributes(xml).collectFirst { case (n, v) if n.is(XmlAttribute.XmlId) => v }.contains("x"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asText) == Seq("a"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asCData) == Seq("b"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asComment) == Seq("c"))
  }

  test("parseHtml into ZioBlocksHtml keeps tags and text and drops comments") {
    val html: ZioBlocksHtml.Element = XmlParser.parseHtml[ZioBlocksHtml.Element]("<p>a<!--c--><b>d</b></p>").toOption.get
    assert(html.getName.is(XmlElement.P))
    assert(html.getChildren.flatMap(_.asComment).isEmpty)
    assert(html.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("b"))
    assert(html.getText == "ad")
  }

  test("parseXml keeps text and CDATA as distinct children") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<![CDATA[b]]>c</p>").toOption.get
    assert(xml.getChildren.flatMap(_.asText) == Seq("a", "c"))
    assert(xml.getChildren.flatMap(_.asCData) == Seq("b"))
  }

  test("parseXml keeps adjacent CDATA sections distinct") {
    val xml: Xml.Element = XmlParser.parseXml("<p><![CDATA[a]]><![CDATA[b]]></p>").toOption.get
    assert(xml.getChildren.flatMap(_.asCData) == Seq("a", "b"))
  }

  test("parseXml keeps undeclared entity references as text") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a&nbsp;b</p>").toOption.get
    assert(xml.getChildren.flatMap(_.asText).mkString == "a&nbsp;b")
  }

package org.podval.xml

import Html.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite
import java.io.File

final class XmlParserSpec extends AnyFunSuite:
  private def children(element: Xml.Element): Seq[Xml.Element] =
    element.getChildren.flatMap(_.asElement)

  test("string parse does not expand xi:include") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<includer>
        |  <xi:include xmlns:xi="http://www.w3.org/2001/XInclude" href="includee.xml"/>
        |</includer>""".stripMargin
    ).toOption.get
    assert(xml.qName == "includer")
    assert(children(xml).map(_.localName) == Seq("include"))
  }

  test("resource parse does not expand xi:include") {
    val xml: Xml.Element = XmlParser.parseResource(classOf[XmlParserSpec], "includer.xml").toOption.get
    assert(children(xml).map(_.localName) == Seq("include"))
    assert(children(xml).flatMap(_.get(XmlAttribute.Href)) == Seq("includee.xml"))
  }

  test("parseXml from URL") {
    val url = classOf[XmlParserSpec].getResource("includee.xml")
    val xml: Xml.Element = XmlParser.parseXml(url).toOption.get
    assert(xml.qName == "includee")
  }

  test("parseXml from File") {
    val url = classOf[XmlParserSpec].getResource("includee.xml")
    assert(url.getProtocol == "file")
    val xml: Xml.Element = XmlParser.parseXml(File(url.toURI)).toOption.get
    assert(xml.qName == "includee")
  }

  test("missing resource is Left") {
    val result: Either[Throwable, Xml.Element] =
      XmlParser.parseResource("/org/podval/xml/no-such.xml")
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Resource not found"))
  }

  test("parseXml does not attach comments outside the root element") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<?xml version="1.0"?>
        |<!-- prologue -->
        |<Day><names/></Day>""".stripMargin
    ).toOption.get
    assert(xml.qName == "Day")
    assert(children(xml).map(_.qName) == Seq("names"))
  }

  test("parseHtml from URL matches string parse") {
    val fromString: Xml.Element = XmlParser.parseHtml("<p>a<b>c</b></p>").toOption.get
    val url = classOf[XmlParserSpec].getResource("fragment.html")
    val fromUrl: Xml.Element = XmlParser.parseHtml(url).toOption.get
    assert(fromUrl.qName == fromString.qName)
    assert(fromUrl.qName == "p")
    assert(children(fromUrl).map(_.qName) == Seq("b"))
    assert(fromUrl.getText == fromString.getText)
  }

  test("keeps text on both sides of a comment") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<!--c-->b</p>").toOption.get
    assert(xml.getChildren.flatMap(_.asText) == Seq("a", "b"))
    assert(xml.getChildren.flatMap(_.asComment) == Seq("c"))
    assert(xml.getChildren.flatMap(_.asElement).isEmpty)
  }

  test("keeps a processing instruction as a child") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<?pi d?>b</p>").toOption.get
    assert(xml.getChildren.flatMap(_.asText) == Seq("a", "b"))
    assert(xml.getChildren.flatMap(_.asProcessingInstruction) == Seq(("pi", "d")))
  }

  test("parseXml into ScalaXml keeps names, text, CDATA, and comments") {
    val xml: ScalaXml.Element = XmlParser.parseXml("""<p xml:id="x">a<![CDATA[b]]><!--c--></p>""").toOption.get
    assert(ScalaXml.qName(xml) == "p")
    assert(ScalaXml.get(xml)(XmlAttribute.XmlId).contains("x"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asText) == Seq("a"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asCData) == Seq("b"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asComment) == Seq("c"))
  }

  test("parseHtml into Html keeps tags and text and drops comments") {
    val html: Html.Element = XmlParser.parseHtml("<p>a<!--c--><b>d</b></p>").toOption.get
    assert(html.qName == "p")
    assert(html.getChildren.flatMap(_.asComment).isEmpty)
    assert(html.getChildren.flatMap(_.asElement).map(_.qName) == Seq("b"))
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

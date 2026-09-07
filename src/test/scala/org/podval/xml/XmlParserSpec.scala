package org.podval.xml

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
    assert(xml.getName == "includer")
    assert(children(xml).map(_.localName) == Seq("include"))
  }

  test("resource parse does not expand xi:include by default") {
    val xml: Xml.Element = XmlParser.parseResource(classOf[XmlParserSpec], "includer.xml").toOption.get
    assert(children(xml).map(_.localName) == Seq("include"))
    assert(children(xml).flatMap(_.get(XmlAttribute.Href)) == Seq("includee.xml"))
  }

  test("xinclude expands one level and sets xml:base relative to the initial document") {
    val xml: Xml.Element = XmlParser.parseResource(
      classOf[XmlParserSpec],
      "includer.xml",
      xinclude = true
    ).toOption.get
    val included: Xml.Element = children(xml).head
    assert(included.getName == "includee")
    assert(included.get(XmlAttribute.XmlBase).contains("includee.xml"))
    assert(children(included).map(_.getName) == Seq("content"))
    assert(children(included).head.getText.trim == "Blah!")
  }

  test("nested xinclude xml:base is relative to the initial document") {
    val site: Xml.Element = XmlParser.parseResource(
      classOf[XmlParserSpec],
      "site/site.xml",
      xinclude = true
    ).toOption.get
    assert(site.getName == "site")
    val books: Xml.Element = children(site).head
    assert(books.getName == "store")
    assert(books.get(XmlAttribute.XmlBase).contains("archive/books.xml"))
    val derzhavin: Xml.Element = children(books).head
    assert(derzhavin.getName == "store")
    assert(derzhavin.get(XmlAttribute.XmlBase).contains("archive/books/book/derzhavin.xml"))
    val volume: Xml.Element = children(derzhavin).head
    assert(volume.getName == "collection")
    assert(volume.get(XmlAttribute.XmlBase).contains("archive/books/book/derzhavin/volume/6.xml"))
    assert(volume.get("n").contains("6"))
    assert(volume.get("pageType").contains("book"))
  }

  test("parseXml from URL") {
    val url = classOf[XmlParserSpec].getResource("includee.xml")
    val xml: Xml.Element = XmlParser.parseXml(url).toOption.get
    assert(xml.getName == "includee")
  }

  test("parseXml from File") {
    val url = classOf[XmlParserSpec].getResource("includee.xml")
    assert(url.getProtocol == "file")
    val xml: Xml.Element = XmlParser.parseXml(File(url.toURI)).toOption.get
    assert(xml.getName == "includee")
  }

  test("missing resource is Left") {
    val result: Either[Throwable, Xml.Element] =
      XmlParser.parseResource("/org/podval/xml/no-such.xml")
    assert(result.isLeft)
    assert(result.swap.toOption.get.getMessage.contains("Resource not found"))
  }

  test("comment before the root element is ignored") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<?xml version="1.0"?>
        |<!-- prologue -->
        |<Day><names/></Day>""".stripMargin
    ).toOption.get
    assert(xml.getName == "Day")
    assert(children(xml).map(_.getName) == Seq("names"))
  }

  test("parseHtml from URL matches string parse") {
    val fromString: Xml.Element = XmlParser.parseHtml("<p>a<b>c</b></p>").toOption.get
    val url = classOf[XmlParserSpec].getResource("fragment.html")
    val fromUrl: Xml.Element = XmlParser.parseHtml(url).toOption.get
    assert(fromUrl.getName == fromString.getName)
    assert(fromUrl.getName == "p")
    assert(children(fromUrl).map(_.getName) == Seq("b"))
    assert(fromUrl.getText == fromString.getText)
  }

  test("missing include target is Left") {
    val result: Either[Throwable, Xml.Element] =
      XmlParser.parseResource(classOf[XmlParserSpec], "includer-missing.xml", xinclude = true)
    assert(result.isLeft)
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
    assert(ScalaXml.getName(xml) == "p")
    assert(ScalaXml.get(xml)("xml:id").contains("x"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asText) == Seq("a"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asCData) == Seq("b"))
    assert(ScalaXml.getChildren(xml).flatMap(ScalaXml.asComment) == Seq("c"))
  }

  test("parseHtml into Html keeps tags and text and drops comments") {
    val html: Html.Element = XmlParser.parseHtml("<p>a<!--c--><b>d</b></p>").toOption.get
    assert(html.getName == "p")
    assert(html.getChildren.flatMap(_.asComment).isEmpty)
    assert(html.getChildren.flatMap(_.asElement).map(_.getName) == Seq("b"))
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

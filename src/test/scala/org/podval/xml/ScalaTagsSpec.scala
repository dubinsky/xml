package org.podval.xml

import Xml.given
import ScalaTags.given
import org.scalatest.funsuite.AnyFunSuite
import scalatags.Text.all.*

final class ScalaTagsSpec extends AnyFunSuite:
  private val tei: String = "http://www.tei-c.org/ns/1.0"

  private def attr(element: ScalaTags.Element, attribute: XmlAttribute): Option[String] =
    element.getAttributes.collectFirst { case (name, value) if name.is(attribute) => value }

  private def attr(element: ScalaTags.Element, name: String): Option[String] =
    element.getAttributes.collectFirst { case (n, value) if n.matches(name) => value }

  test("element uses the given name") {
    val el: ScalaTags.Element = ScalaTags.element(XmlElement.P)
    assert(el.getName.qName == "p")
    assert(el.tag == "p")
    assert(el.toString == "<p></p>")
  }

  test("element takes attributes and children") {
    val el: ScalaTags.Element = ScalaTags.element(
      "p",
      Seq("xml:id" -> "x"),
      Seq(ScalaTags.text("a<b"))
    )
    assert(el.getName.qName == "p")
    assert(XmlName.asPairs(el.getAttributes) == Seq("xml:id" -> "x"))
    assert(el.getChildren.flatMap(_.asText) == Seq("a<b"))
    assert(el.getChildren.flatMap(_.asCData).isEmpty)
    assert(el.toString == """<p xml:id="x">a&lt;b</p>""")
  }

  test("attributes preserve order") {
    val el: ScalaTags.Element = ScalaTags.element(
      "p",
      Seq("id" -> "a", "xml:id" -> "b", "class" -> "c"),
      Seq.empty
    )
    assert(XmlName.asPairs(el.getAttributes) == Seq("id" -> "a", "xml:id" -> "b", "class" -> "c"))
  }

  test("qualified names keep a declared URI") {
    val el: ScalaTags.Element = ScalaTags.element(
      "tei:p",
      Seq("xmlns:tei" -> tei, "xml:id" -> "n1"),
      Seq(ScalaTags.text("a"))
    )
    assert(el.getName.qName == "tei:p")
    assert(el.getName.uri.contains(tei))
    assert(attr(el, XmlAttribute.XmlId).contains("n1"))
    val renamed: ScalaTags.Element = ScalaTags.withName(el, "div")
    assert(renamed.getName.qName == "div")
    assert(renamed.getName.uri.isEmpty)
  }

  test("cdata becomes text") {
    val node: ScalaTags.Node = ScalaTags.cdata("a<b")
    assert(node.asText.contains("a<b"))
    assert(node.asCData.isEmpty)
    assert(node.asAtom.contains("a<b"))
  }

  test("to drops comments and processing instructions and turns CDATA into text") {
    val xml: Xml.Element = XmlParser.parseXml("<p>a<!--c--><?pi d?>b<![CDATA[a<b]]></p>").toOption.get
    val tags: ScalaTags.Element = xml.to[ScalaTags.Element]
    assert(tags.getChildren.flatMap(_.asText) == Seq("a", "b", "a<b"))
    assert(tags.getChildren.flatMap(_.asComment).isEmpty)
    assert(tags.getChildren.flatMap(_.asProcessingInstruction).isEmpty)
    val round: Xml.Element = tags.to[Xml.Element]
    assert(round.getChildren.flatMap(_.asText) == Seq("a", "b", "a<b"))
    assert(round.getChildren.flatMap(_.asCData).isEmpty)
    assert(round.getChildren.flatMap(_.asComment).isEmpty)
  }

  test("to round-trips mixed content, attributes, and an inherited namespace") {
    val xml: Xml.Element = XmlParser.parseXml(
      s"""<tei:p xmlns:tei="$tei" xml:id="n1"><tei:hi>a</tei:hi>b</tei:p>"""
    ).toOption.get
    val tags: ScalaTags.Element = xml.to[ScalaTags.Element]
    assert(tags.getName.qName == "tei:p")
    assert(tags.getName.uri.contains(tei))
    assert(attr(tags, XmlAttribute.XmlId).contains("n1"))
    val hi: ScalaTags.Element = tags.getChildren.flatMap(_.asElement).head
    assert(hi.getName.qName == "tei:hi")
    assert(hi.getName.uri.contains(tei))
    val round: Xml.Element = tags.to[Xml.Element]
    assert(round.getName.qName == "tei:p")
    assert(round.getName.uri.contains(tei))
    assert(round.get(XmlAttribute.XmlId).contains("n1"))
    assert(round.get(XmlAttribute.Xmlns("tei")).contains(tei))
    val roundHi: Xml.Element = round.childElements.head
    assert(roundHi.getName.qName == "tei:hi")
    assert(roundHi.getName.uri.contains(tei))
    assert(round.getChildren.flatMap(_.asText) == Seq("b"))
  }

  test("to keeps a prefixed attribute URI without xmlns on that element") {
    val xml: Xml.Element = XmlParser.parseXml(
      s"""<p xmlns:xlink="${XmlNamespace.xlink.uri}"><ref xlink:href="a.xml"/></p>"""
    ).toOption.get
    val round: Xml.Element = xml.to[ScalaTags.Element].to[Xml.Element]
    val ref: Xml.Element = round.childElements.head
    assert(ref.getName.qName == "ref")
    val href: XmlName = ref.getAttributes.collect { case (name, _) if name.qName == "xlink:href" => name }.head
    assert(href.uri.contains(XmlNamespace.xlink.uri))
    assert(ref.get("xlink:href").contains("a.xml"))
  }

  test("a ScalaTags tree reads tags, styles, and text") {
    val el: ScalaTags.Element = div(id := "x", cls := "y", color := "red", "a", p("b"))
    assert(el.getName.qName == "div")
    assert(attr(el, "id").contains("x"))
    assert(attr(el, XmlAttribute.CssClass).contains("y"))
    assert(attr(el, "style").contains("color: red;"))
    assert(el.getChildren.flatMap(_.asText) == Seq("a"))
    assert(el.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("p"))
    assert(el.getChildren.flatMap(_.asElement).head.getText == "b")
  }

  test("raw markup is dropped") {
    val el: ScalaTags.Element = div(raw("<b>a</b>"), "c")
    assert(el.getChildren.flatMap(_.asText) == Seq("c"))
    assert(el.getChildren.flatMap(_.asElement).isEmpty)
    val round: Xml.Element = el.to[Xml.Element]
    assert(round.getChildren.flatMap(_.asText) == Seq("c"))
  }

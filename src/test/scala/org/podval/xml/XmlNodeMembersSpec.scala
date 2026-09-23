package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class XmlNodeMembersSpec extends AnyFunSuite:
  test("getChildren, setChildren, fold, and isElement need no Xml.given") {
    val span: Xml.Element = Xml.element(XmlElement.Span)
    val element: Xml.Element = Xml.element(XmlElement.P).setChildren(Seq(
      Xml.text("a"),
      Xml.cdata("b"),
      Xml.comment("c").get,
      Xml.processingInstruction("pi", "d").get,
      span
    ))
    assert(element.getChildren.flatMap(_.asText) == Seq("a"))
    assert(element.getChildren.flatMap(_.asElement) == Seq(span))
    assert(element.isElement(XmlElement.P))
    assert(!element.isElement(XmlElement.Div))
    assert(element.isNamed("p"))
    assert(!element.isNamed("div"))

    val replaced: Xml.Element = element.setChildren(Seq(Xml.text("z")))
    assert(replaced.getChildren.flatMap(_.asText) == Seq("z"))
    assert(element.getChildren.flatMap(_.asText) == Seq("a"))
    assert(replaced.setText("ab").getChildren.flatMap(_.asText) == Seq("ab"))

    val kinds: Seq[String] = element.getChildren.map: node =>
      node.fold(
        element = _ => "el",
        text = _ => "text",
        cdata = _ => "cdata",
        comment = _ => "comment",
        processingInstruction = (_, _) => "pi",
        unknown = "unknown"
      )
    assert(kinds == Seq("text", "cdata", "comment", "pi", "el"))

    val rich: Xml.Element = Xml.element(XmlElement.P).setChildren(Seq(
      Xml.text("a"),
      Xml.element(XmlElement.Em).setText("b"),
      Xml.text(", c")
    ))
    assert(rich.getText == "ab, c")
    assert(Xml.text("  ").isWhitespace)
    assert(!Xml.cdata("  ").isWhitespace)
    assert(Xml.cdata("  ").isCharacters)
    assert(!Xml.text("  ").isCharacters)
    assert(Xml.text("a").asAtom.contains("a"))
    assert(Xml.cdata("b").asAtom.contains("b"))
    assert(Xml.comment("c").get.asAtom.isEmpty)
  }

  test("Xml extensions delegate to XmlNode members") {
    import Xml.given

    val element: Xml.Element = Xml.element(XmlElement.P).setChildren(Seq(
      Xml.text("a"),
      Xml.element(XmlElement.Span)
    ))

    def via[E: XmlAst](element: E): (Int, Boolean, String, String) =
      val replaced: E = element.setChildren(element.getChildren.take(1))
      val kind: String = element.getChildren.head.fold(
        element = _ => "el",
        text = _ => "text",
        cdata = _ => "cdata",
        comment = _ => "comment",
        processingInstruction = (_, _) => "pi",
        unknown = "unknown"
      )
      (replaced.getChildren.length, element.isElement(XmlElement.P), kind, element.getText)

    assert(via(element) == (1, true, "text", "a"))
    assert(element.getChildren.length == 2)
  }

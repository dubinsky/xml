package org.podval.xml

import Xml.given
import ScalaXml.given
import org.scalatest.funsuite.AnyFunSuite

final class XmlDocumentSpec extends AnyFunSuite:
  private def parseDoc(content: String): XmlDocument[Xml.Element] =
    XmlParser.parseXmlDocument(content).toOption.get

  private def render(document: XmlDocument[Xml.Element]): String =
    XmlWriterConfig.Plain.render(document)

  test("parseXml still returns only the document element") {
    val xml: Xml.Element = XmlParser.parseXml(
      """<?xml version="1.0"?>
        |<!-- prologue -->
        |<Day><names/></Day>
        |<!-- epilogue -->""".stripMargin
    ).toOption.get
    assert(xml.isNamed("Day"))
    assert(xml.getChildren.flatMap(_.asComment).isEmpty)
    assert(xml.childElements.map(_.getName.qName) == Seq("names"))
  }

  test("parseXmlDocument keeps prolog and epilogue comments") {
    val doc: XmlDocument[Xml.Element] = parseDoc(
      """<?xml version="1.0"?>
        |<!-- prologue -->
        |<Day><names/></Day>
        |<!-- epilogue -->""".stripMargin
    )
    assert(doc.root.isNamed("Day"))
    assert(doc.declaration.contains(XmlDeclaration()))
    assert(doc.doctype.isEmpty)
    assert(doc.prolog == Seq(XmlNode.Comment(" prologue ")))
    assert(doc.epilogue == Seq(XmlNode.Comment(" epilogue ")))
  }

  test("parseXmlDocument keeps prolog and epilogue processing instructions") {
    val doc: XmlDocument[Xml.Element] = parseDoc(
      """<?xml-stylesheet href="a.css"?>
        |<p/>
        |<?pi d?>""".stripMargin
    )
    assert(doc.prolog == Seq(XmlNode.ProcessingInstruction("xml-stylesheet", "href=\"a.css\"")))
    assert(doc.epilogue == Seq(XmlNode.ProcessingInstruction("pi", "d")))
  }

  test("parseXmlDocument keeps a doctype") {
    val doc: XmlDocument[Xml.Element] = parseDoc(
      """<?xml version="1.0"?>
        |<!DOCTYPE Day>
        |<Day/>""".stripMargin
    )
    assert(doc.doctype.contains(XmlDoctype("Day")))
  }

  test("parseXmlDocument keeps PUBLIC and SYSTEM doctype ids") {
    val doc: XmlDocument[Xml.Element] = parseDoc(
      """<?xml version="1.0"?>
        |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
        |<html/>""".stripMargin
    )
    assert(doc.doctype.contains(XmlDoctype(
      name = "html",
      publicId = Some("-//W3C//DTD XHTML 1.0 Strict//EN"),
      systemId = Some("http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
    )))
  }

  test("document write emits the canonical declaration") {
    val dumped: String = render(parseDoc("<p>a</p>"))
    assert(dumped.startsWith("""<?xml version="1.0" encoding="UTF-8"?>"""), dumped)
  }

  test("document round-trip keeps prolog comment, PI, doctype, and epilogue") {
    val input: String =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<!DOCTYPE Day>
        |<!--
        |  file comment
        |-->
        |<?keep me?>
        |<Day><names/></Day>
        |<!-- after -->
        |""".stripMargin
    val doc: XmlDocument[Xml.Element] = parseDoc(input)
    assert(doc.doctype.contains(XmlDoctype("Day")))
    assert(doc.prolog == Seq(
      XmlNode.Comment("\n  file comment\n"),
      XmlNode.ProcessingInstruction("keep", "me")
    ))
    assert(doc.epilogue == Seq(XmlNode.Comment(" after ")))
    val dumped: String = render(doc)
    val round: XmlDocument[Xml.Element] = parseDoc(dumped)
    assert(round.declaration.contains(XmlDeclaration()), dumped)
    assert(round.doctype == doc.doctype, dumped)
    assert(round.prolog == doc.prolog, dumped)
    assert(round.epilogue == doc.epilogue, dumped)
    assert(round.root.isNamed("Day"), dumped)
  }

  test("Haftarah-shaped prologue comment survives parse/write") {
    val input: String =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<!--
        |  Each Haftarah is described by the <week> element.
        |-->
        |<Haftarah>
        |    <week n="Bereishis"/>
        |</Haftarah>
        |""".stripMargin
    val dumped: String = render(parseDoc(input))
    val round: XmlDocument[Xml.Element] = parseDoc(dumped)
    assert(round.prolog == Seq(XmlNode.Comment("\n  Each Haftarah is described by the <week> element.\n")))
    assert(dumped.startsWith("""<?xml version="1.0" encoding="UTF-8"?>"""))
    assert(dumped.contains("Each Haftarah is described"))
  }

  test("ScalaXml document parse keeps prolog comments") {
    val doc: XmlDocument[ScalaXml.Element] = XmlParser.parseXmlDocument(
      "<!-- c --><p>a</p>"
    ).toOption.get
    assert(doc.prolog == Seq(XmlNode.Comment(" c ")))
    assert(doc.root.getName.qName == "p")
  }

  test("XmlDocument.xml helper sets the canonical declaration") {
    val xml: Xml.Element = Xml.element(XmlElement.P)
    val doc: XmlDocument[Xml.Element] = XmlDocument.xml(
      root = xml,
      prolog = Seq(XmlNode.Comment(" c "))
    )
    val dumped: String = render(doc)
    assert(dumped.startsWith("""<?xml version="1.0" encoding="UTF-8"?>"""), dumped)
    assert(dumped.contains("<!-- c -->"), dumped)
  }

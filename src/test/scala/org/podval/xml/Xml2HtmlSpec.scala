package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class Xml2HtmlSpec extends AnyFunSuite:
  private def parse(xml: String): Xml.Element =
    XmlParser.parseXml(xml).toOption.get

  test("unprefixed reserved element is prefixed") {
    val html: Xml.Element = Xml2Html("tei").convert(parse("<p>a</p>"))
    assert(html.qName == "tei-p")
    assert(html.getClasses.contains("p"))
  }

  test("prefixed reserved element is rewritten by local name") {
    val html: Xml.Element = Xml2Html("tei").convert(
      parse("""<tei:p xmlns:tei="http://www.tei-c.org/ns/1.0">a</tei:p>""")
    )
    assert(html.qName == "tei-p")
    assert(html.getClasses.contains("p"))
  }

  test("reserved attributes use local names; xml:lang and xml:id stay") {
    val html: Xml.Element = Xml2Html("tei").convert(
      parse("""<p xml:id="n1" xml:lang="en" lang="fr" class="x"/>""")
    )
    assert(html.get(XmlAttribute.XmlId).contains("n1"))
    assert(html.get(XmlAttribute.XmlLang).contains("en"))
    assert(html.get("tei-lang").contains("fr"))
    assert(html.get(XmlAttribute.Lang).isEmpty)
    assert(html.get("tei-class").contains("x"))
    assert(html.getClasses.contains("p"))
  }

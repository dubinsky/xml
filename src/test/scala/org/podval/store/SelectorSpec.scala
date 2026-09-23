package org.podval.store

import org.podval.metadata.{Name, Names}
import org.podval.xml.{XmlParser, Xml}
import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  private val catalog: Selectors = Selectors(
    Selector(Names("book")),
    Selector(
      Names(Seq(
        Name("names", org.podval.metadata.Language.English.toSpec),
        Name("имена", org.podval.metadata.Language.Russian.toSpec)
      )),
      plural = Some(Names(Seq(Name("Имена", org.podval.metadata.Language.Russian.toSpec))))
    ),
    Selector(Names("inventory"))
  )

  test("Selectors.forName and getForName") {
    assert(catalog.forName("book").isDefined)
    assert(catalog.getForName("inventory").names.hasName("inventory"))
    assert(catalog.getForName("names").plural.exists(_.hasName("Имена")))
    assert(catalog.getForName("names").pluralOrNames.hasName("Имена"))
    assert(catalog.getForName("inventory").plural.isEmpty)
    assert(catalog.getForName("inventory").pluralOrNames.hasName("inventory"))
    assert(catalog.forName("item").isEmpty)
    intercept[IllegalArgumentException] { catalog.getForName("item") }
  }

  test("Selector codec") {
    def decode(xml: String) =
      Selector.codec.decode(XmlParser.parseXml(xml).toOption.get)

    val book = decode("""<selector><name lang="en" n="book"/></selector>""").toOption.get
    assert(book.names.hasName("book"))
    val encoded = Selector.codec.encode(book)
    assert(Selector.codec.decode(encoded).toOption.get.names.hasName("book"))
  }

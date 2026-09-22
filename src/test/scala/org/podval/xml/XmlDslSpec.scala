package org.podval.xml

import org.podval.xml.dsl.{*, given}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.html.Dom

final class XmlDslSpec extends AnyFunSuite:
  private def render(element: ZioBlocksXml.Element): String =
    HtmlXmlWriterConfig.render(element)

  private def qNames(element: ZioBlocksXml.Element): Seq[(String, String)] =
    XmlName.asPairs(element.getAttributes)

  test("Option, Seq, nested Option in Seq, and String plus element children flatten") {
    val nested: Seq[Option[ZioBlocksXml.Element]] = Seq(Some(span("a")), None, Some(em("b")))
    val el: ZioBlocksXml.Element = div(
      Option.when(true)(p("p")),
      Option.empty[String],
      nested,
      "text",
      code("c")
    )
    val children: Seq[String] = el.getChildren.map: node =>
      node.fold(
        element = e => e.getName.qName,
        text = identity,
        cdata = identity,
        comment = identity,
        processingInstruction = (target, _) => target,
        unknown = "?"
      )
    assert(children == Seq("p", "span", "em", "text", "code"))
  }

  test("later className := replaces earlier +=") {
    val el: ZioBlocksXml.Element = div(className += "a", className := "b")
    assert(qNames(el) == Seq("class" -> "b"))
    val dumped: String = render(el)
    assert(dumped.contains("""class="b""""), dumped)
    assert(!dumped.contains("b a"), dumped)
  }

  test("hidden := false is omitted; hidden := true is hidden=\"true\"") {
    val hiddenFalse: String = render(div(hidden := false, id := "x"))
    assert(!hiddenFalse.contains("hidden"), hiddenFalse)
    assert(hiddenFalse.contains("""id="x""""), hiddenFalse)
    val hiddenTrue: String = render(div(hidden := true))
    assert(hiddenTrue.contains("""hidden="true""""), hiddenTrue)
    val unset: String = render(div(hidden := true, hidden := false, id := "x"))
    assert(!unset.contains("hidden"), unset)
    assert(unset.contains("""id="x""""), unset)
  }

  test("attributes stay in insertion order, not alphabetical") {
    val el: ZioBlocksXml.Element = div(id := "x", className := "a")
    assert(qNames(el) == Seq("id" -> "x", "class" -> "a"))
    val dumped: String = render(el)
    assert(dumped.indexOf("id=") < dumped.indexOf("class="), dumped)
  }

  test("http-equiv, itemscope=\"true\", and itemtype qNames") {
    val metaEl: String = render(meta(httpEquiv := "X-UA-Compatible", contentAttr := "IE=edge"))
    assert(metaEl.contains("http-equiv"), metaEl)
    assert(metaEl.contains("""content="IE=edge""""), metaEl)
    val articleEl: String = render(
      article(itemScope := true, itemType := "http://schema.org/WebPage")
    )
    assert(articleEl.contains("""itemscope="true""""), articleEl)
    assert(articleEl.contains("itemtype"), articleEl)
    assert(articleEl.contains("http://schema.org/WebPage"), articleEl)
  }

  test(".when(true) merges into existing lang and body") {
    val el: ZioBlocksXml.Element = html(langAttr := "en", body("x")).when(true)(className := "wide")
    assert(qNames(el) == Seq("lang" -> "en", "class" -> "wide"))
    assert(el.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("body"))
  }

  test(".when(false) is identity") {
    val el: ZioBlocksXml.Element = html(langAttr := "en", body())
    assert(el.when(false)(className := "wide") eq el)
  }

  test("inlineJs appends text; externalJs sets src; module type") {
    val inline: ZioBlocksXml.Element = script().inlineJs("if (a < b) {}")
    assert(inline.getChildren.flatMap(_.asText) == Seq("if (a < b) {}"))
    val twice: ZioBlocksXml.Element = script().inlineJs("a();").inlineJs("b();")
    assert(twice.getChildren.flatMap(_.asText) == Seq("a();", "b();"))
    val external: ZioBlocksXml.Element = script().externalJs("https://example.com/x.js")
    assert(qNames(external) == Seq("src" -> "https://example.com/x.js"))
    val module: ZioBlocksXml.Element = script(typeAttr := "module").inlineJs("x()")
    assert(qNames(module) == Seq("type" -> "module"))
    assert(module.getChildren.flatMap(_.asText) == Seq("x()"))
  }

  test("HtmlXmlWriterConfig.render: void br; raw script with < and newline") {
    assert(render(br()).contains("<br/>"))
    val dumped: String = render(script().inlineJs("if (a < b)\n{}"))
    assert(dumped.contains("if (a < b)\n{}"), dumped)
    assert(!dumped.contains("&lt;"), dumped)
  }

  test("ZioBlocksHtml.div with ZioBlocksHtml.given still yields Generic") {
    import ZioBlocksHtml.given
    import ZioBlocksHtml.ToXmlMod.given
    val el: ZioBlocksHtml.Element = ZioBlocksHtml.div(ZioBlocksHtml.className := "x", "y")
    assert(el.isInstanceOf[Dom.Element.Generic])
    assert(el.getName.qName == "div")
    assert(XmlName.asPairs(el.getAttributes) == Seq("class" -> "x"))
  }

  test("element(name, mods*) builds names without a tag function") {
    val el: ZioBlocksXml.Element = element("urlset", xmlns := "http://www.sitemaps.org/schemas/sitemap/0.9", "x")
    assert(el.isNamed("urlset"))
    assert(el.get(XmlAttribute.Xmlns).contains("http://www.sitemaps.org/schemas/sitemap/0.9"))
    assert(el.getChildren.flatMap(_.asText) == Seq("x"))
  }

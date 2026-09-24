package org.podval.xml

import Xml.given
import ZioBlocksHtml.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.html.*

final class HtmlAttributesSpec extends AnyFunSuite:
  private def qNames(el: ZioBlocksHtml.Element): Seq[(String, String)] =
    XmlName.asPairs(el.getAttributes)

  test("className += values merge into a single class attribute for XmlWriter") {
    val el: ZioBlocksHtml.Element = span(
      className += "icon-span",
      className += "grey fa-classic fa-regular",
      className += "fa-file"
    )
    val attrs = qNames(el)
    assert(attrs.count(_._1 == "class") == 1)
    assert(attrs.find(_._1 == "class").map(_._2).contains("icon-span grey fa-classic fa-regular fa-file"))
    val rendered = HtmlXmlWriterConfig.render(el.to[Xml.Element])
    assert(rendered.contains("""class="icon-span grey fa-classic fa-regular fa-file""""))
    assert(!rendered.matches("""(?s).*class="[^"]*".*class=".*"""))
  }

  test(":= base then += appends for any attribute name") {
    val el = div(className := "base", className += "extra")
    assert(qNames(el) == Seq("class" -> "base extra"))
  }

  test("duplicate KeyValue last wins without AppendValue") {
    val el = div(className := "a", className := "b")
    assert(qNames(el) == Seq("class" -> "b"))
  }

  test("last := is the base; earlier += are still appended") {
    val el = div(className += "x", className := "b", className += "y")
    assert(qNames(el) == Seq("class" -> "b x y"))
  }

  test("merged class string matches Dom.render") {
    def classOf(html: String): String =
      """class="([^"]*)"""".r.findFirstMatchIn(html).map(_.group(1)).getOrElse("")

    def check(el: ZioBlocksHtml.Element): Unit =
      val fromGet = el.getAttributes.collectFirst { case (n, v) if n.is(XmlAttribute.CssClass) => v }.getOrElse("")
      assert(qNames(el).count(_._1 == "class") == 1, el.render)
      assert(fromGet == classOf(el.render), s"get=$fromGet render=${el.render}")
      val xmlRendered: String = HtmlXmlWriterConfig.render(el.to[Xml.Element])
      assert(fromGet == classOf(xmlRendered), s"get=$fromGet xml=$xmlRendered")

    check(span(className += "icon-span", className += "grey fa-classic fa-regular", className += "fa-file"))
    check(div(className := "base", className += "extra"))
    check(div(className := "a", className := "b"))
    check(div(className += "x", className := "b", className += "y"))
    check(div(className += "a", id := "x", className := "b"))
    check(div(className := ("a", "b"), className += "c"))
    check(div(id := "x", hidden := true, className := "a", className += "b"))
  }

  test("attributes are emitted in alphabetical name order") {
    val el = div(id := "x", hidden := true, className := "a")
    assert(qNames(el) == Seq(
      "class" -> "a",
      "hidden" -> "true",
      "id" -> "x"
    ))
  }

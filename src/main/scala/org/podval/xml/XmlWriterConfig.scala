package org.podval.xml

object XmlWriterConfig:
  object Plain extends XmlWriterConfig()

// Describes how to write an XML dialect. Element-name sets match local names.
open class XmlWriterConfig(
  val preformat: Set[String] = Set.empty,
  // HTML raw-text: preserve layout like preformat, do not entity-encode text, break `</`.
  val rawText: Set[String] = Set.empty,
  val stack: Set[String] = Set.empty,
  // Phrasing: do not indent children, and glue to the previous element (same as cling).
  val unStack: Set[String] = Set.empty,
  val nest: Set[String] = Set.empty,
  val break: Set[String] = Set.empty,
  // Glue to the previous element even when both sides are elements.
  val cling: Set[String] = Set.empty,
  // Empty tags: local names written as <br/>; others as <script></script>.
  val selfClose: Set[String] = Set.empty,
  // Every empty element is `<e/>`. `selfClose` still applies when this is false.
  val selfCloseEmpty: Boolean = false,
  // Keep the tags on the content's first and last line. Does not cling to the previous element.
  val stick: Set[String] = Set.empty
):
  def render[Element: XmlAst](element: Element): String =
    render(element, XmlWriter.widthDefault)

  def render[Element: XmlAst](element: Element, width: Int): String =
    XmlWriter.render(this, element, width)

  def render[Element: XmlAst](document: XmlDocument[Element]): String =
    render(document, XmlWriter.widthDefault)

  def render[Element: XmlAst](document: XmlDocument[Element], width: Int): String =
    XmlWriter.render(this, document, width)

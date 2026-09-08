package org.podval.xml

object Xml2Html:
  // Names that cannot be reused as-is when TEI (or similar) is serialized as HTML:
  // - `head`/`body`/`title` are HTML document chrome.
  // - HTML `p` is phrasing-content only. The parser auto-closes it before any block
  //   (`p`, `div`, `ul`/`ol`/`dl`, `table`, `blockquote`, `figure`, headings, …).
  //   TEI `p` may contain those, and footnote/glossary tips copy body nodes that may
  //   too, so it is renamed (`tei-p`). TEI `div` stays `div`.
  private val reservedHtmlElements: Set[String] = Set("head", "body", "title", "p")

  private val reservedAttributes: Set[String] = Set(
    XmlAttribute.CssClass.qName,
    XmlAttribute.Target.qName,
    XmlAttribute.Lang.qName,
    "frame"
  )

// Prefix attribute and element names that collide with the HTML ones.
// Element and attribute sets match local names; `xml:*` is left as-is.
// Namespace-based styling didn't work: browser DOM elements seem to be
// in the HTML5 xhtml namespace unless `xmlns` attribute is present
// on that very element (no inheritance).
// TODO maybe this should be a method?
final class Xml2Html(prefix: String):
  private def withPrefix(name: String): String = s"$prefix-$name"

  def convert[E: XmlAst](element: E): E =
    val attributesConverted: E = element.setAttributes(element.getExpandedAttributes.map((name, value) =>
      val nameNew: String =
        if name.isXml
        then name.qName
        else if !Xml2Html.reservedAttributes.contains(name.localName)
        then name.qName
        else withPrefix(name.localName)
      (nameNew, value)
    ))

    if !Xml2Html.reservedHtmlElements.contains(element.localName)
    then attributesConverted
    else attributesConverted.renameKeepingClass(withPrefix(element.localName))

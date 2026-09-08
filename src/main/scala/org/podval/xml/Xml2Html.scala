package org.podval.xml

object Xml2Html:
  // Names that cannot be reused as-is when TEI (or similar) is serialized as HTML:
  // - `head`/`body`/`title` are HTML document chrome.
  // - HTML `p` is phrasing-content only. The parser auto-closes it before any block
  //   (`p`, `div`, `ul`/`ol`/`dl`, `table`, `blockquote`, `figure`, headings, …).
  //   TEI `p` may contain those, and footnote/glossary tips copy body nodes that may
  //   too, so it is renamed (`tei-p`). TEI `div` stays `div`.
  private val reservedHtmlElements: Set[String] = Set("head", "body", "title", "p")

  private val reservedAttributes: Set[String] = Set("class", "target", "lang", "frame")

  def renameElement[E: XmlAst](name: String, element: E): E = element
    .addClass(element.localName)
    .rename(name)


/*
I tried to define CSS namespaces like this:
@namespace tei   url("http://www.tei-c.org/ns/1.0");
@namespace db    url("http://docbook.org/ns/docbook");
@namespace xhtml url("http://www.w3.org/1999/xhtml");
and use them in CSS rules like this: tei|div, docbook|title.

It seems that in browser DOM all elements are in the HTML5 xhtml namespace
unless `xmlns` attribute is present on that element;
why aren't the namespace declarations inherited is not clear.

So, I prefix names that clash with HTML (see Xml2Html.reservedHtmlElements).
In particular HTML `p` cannot contain blocks; TEI `p` can, so it becomes `tei-p`.
*/
// Prefix attribute and element names that collide with the HTML ones.
// Element and attribute sets match local names; `xml:*` is left as-is.
final class Xml2Html(prefix: String):
  private def withPrefix(name: String): String = s"$prefix-$name"

  def convert[E: XmlAst](element: E): E =
    val attributesConverted: E = element.setAttributes(element.getExpandedAttributes.map((name, value) =>
      val nameNew: String =
        if name.namespace.contains(XmlNamespace.xml.uri)
        then name.qualifiedName
        else if !Xml2Html.reservedAttributes.contains(name.localName)
        then name.qualifiedName
        else withPrefix(name.localName)
      (nameNew, value)
    ))

    if !Xml2Html.reservedHtmlElements.contains(element.localName)
    then attributesConverted
    else Xml2Html.renameElement(withPrefix(element.localName), attributesConverted)

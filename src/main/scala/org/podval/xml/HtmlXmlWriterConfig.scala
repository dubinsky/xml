package org.podval.xml

object HtmlXmlWriterConfig extends XmlWriterConfig(
  preformat = Set(XmlElement.Pre.localName),
  stack = Set("nav", "header", "main", XmlElement.Div.localName),
  // Phrasing wrappers: never indent children (that would become a visible HTML space),
  // and glue to the previous element so `<em>a</em><strong>b</strong>` stays inline.
  unStack = Set(
    XmlElement.A.localName, "abbr", "b", "bdi", "bdo", "cite", XmlElement.Code.localName, "data", "del", "dfn",
    XmlElement.Em.localName, "i", "kbd", "mark", "q", "s", "samp", "small", XmlElement.Span.localName, "strong", "sub",
    "sup", "time", "u", "var"
  ),
  break = Set(XmlElement.Br.localName),
  // HTML void elements: no end tag, no content. XmlWriter emits <br/> for these
  // when empty; other empty elements become <script></script> (a self-closed
  // <script/> is mis-parsed). https://html.spec.whatwg.org/multipage/syntax.html#void-elements
  selfClose = Set(
    "area", "base", XmlElement.Br.localName, "col", "embed", "hr", XmlElement.Img.localName, "input",
    "link", "meta", "source", "track", "wbr"
  )
)

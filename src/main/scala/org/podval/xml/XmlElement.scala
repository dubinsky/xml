package org.podval.xml

open class XmlElement(val name: XmlName):
  def this(qName: String) = this(XmlName.parse(qName))

  def qName: String = name.qName

  def localName: String = name.localName

  def matches(other: XmlName): Boolean = other.is(this)

object XmlElement:
  object A extends XmlElement("a")
  object Abbr extends XmlElement("abbr")
  object Area extends XmlElement("area")
  object Article extends XmlElement("article")
  object Aside extends XmlElement("aside")
  object B extends XmlElement("b")
  object Base extends XmlElement("base")
  object Bdi extends XmlElement("bdi")
  object Bdo extends XmlElement("bdo")
  object Blockquote extends XmlElement("blockquote")
  object Body extends XmlElement("body")
  object Br extends XmlElement("br")
  object Cite extends XmlElement("cite")
  object Code extends XmlElement("code")
  object Col extends XmlElement("col")
  object Data extends XmlElement("data")
  object Dd extends XmlElement("dd")
  object Del extends XmlElement("del")
  object Details extends XmlElement("details")
  object Dfn extends XmlElement("dfn")
  object Div extends XmlElement("div")
  object Dl extends XmlElement("dl")
  object Dt extends XmlElement("dt")
  object Em extends XmlElement("em")
  object Embed extends XmlElement("embed")
  object Figcaption extends XmlElement("figcaption")
  object Figure extends XmlElement("figure")
  object Footer extends XmlElement("footer")
  object H1 extends XmlElement("h1")
  object H2 extends XmlElement("h2")
  object H3 extends XmlElement("h3")
  object Head extends XmlElement("head")
  object Header extends XmlElement("header")
  object Hr extends XmlElement("hr")
  object Html extends XmlElement("html")
  object I extends XmlElement("i")
  object Iframe extends XmlElement("iframe")
  object Img extends XmlElement("img")
  object Input extends XmlElement("input")
  object Kbd extends XmlElement("kbd")
  object Label extends XmlElement("label")
  object Li extends XmlElement("li")
  object Link extends XmlElement("link")
  object Main extends XmlElement("main")
  object Mark extends XmlElement("mark")
  object Meta extends XmlElement("meta")
  object Nav extends XmlElement("nav")
  // HTML `object` element. Call sites write `XmlElement.Object`.
  object Object extends XmlElement("object")
  object Ol extends XmlElement("ol")
  object P extends XmlElement("p")
  object Pre extends XmlElement("pre")
  object Q extends XmlElement("q")
  object S extends XmlElement("s")
  object Samp extends XmlElement("samp")
  object Script extends XmlElement("script")
  object Small extends XmlElement("small")
  object Source extends XmlElement("source")
  object Span extends XmlElement("span")
  object Strong extends XmlElement("strong")
  object Style extends XmlElement("style")
  object Sub extends XmlElement("sub")
  object Summary extends XmlElement("summary")
  object Sup extends XmlElement("sup")
  object Table extends XmlElement("table")
  object Td extends XmlElement("td")
  object Th extends XmlElement("th")
  object Time extends XmlElement("time")
  object Title extends XmlElement("title")
  object Tr extends XmlElement("tr")
  object Track extends XmlElement("track")
  object U extends XmlElement("u")
  object Ul extends XmlElement("ul")
  object Var extends XmlElement("var")
  object Video extends XmlElement("video")
  object Wbr extends XmlElement("wbr")

  val preformat: Seq[XmlElement] = Seq(Pre)

  val rawText: Seq[XmlElement] = Seq(Script, Style)

  val stack: Seq[XmlElement] = Seq(Nav, Header, Main, Div)

  // Phrasing wrappers: never indent children (that would become a visible HTML space),
  // and glue to the previous element so `<em>a</em><strong>b</strong>` stays inline.
  val unStack: Seq[XmlElement] = Seq(
    A, Abbr, B, Bdi, Bdo, Cite, Code, Data, Del, Dfn,
    Em, I, Kbd, Mark, Q, S, Samp, Small, Span, Strong, Sub,
    Sup, Time, U, Var
  )

  val break: Seq[XmlElement] = Seq(Br)

  // HTML void elements: no end tag, no content. XmlWriter emits <br/> for these
  // when empty; other empty elements become <script></script> (a self-closed
  // <script/> is mis-parsed). https://html.spec.whatwg.org/multipage/syntax.html#void-elements
  val selfClose: Seq[XmlElement] = Seq(
    Area, Base, Br, Col, Embed, Hr, Img, Input,
    Link, Meta, Source, Track, Wbr
  )

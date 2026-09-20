package org.podval.xml

/** Construction DSL over this AST: tag functions, `:=` / `+=`, `ToXmlMod` flattening, `.when`, script helpers.
  * Inspired by ZIO Blocks HTML (`zio-blocks-html` 0.0.51).
  */
private[xml] trait XmlAstDsl[ELEMENT]:
  this: XmlAst[ELEMENT] =>

  enum XmlMod derives CanEqual:
    case SetAttr(name: XmlName, value: String)
    case AppendAttr(name: XmlName, value: String, separator: String)
    case Child(node: Node)
    case Many(mods: Seq[XmlMod])
    case Empty

  trait ToXmlMod[-A]:
    def toMod(a: A): XmlMod

  trait LowPriorityToXmlMod:
    given iterable: [A] => ToXmlMod[A] => ToXmlMod[Iterable[A]]:
      def toMod(as: Iterable[A]): XmlMod =
        XmlMod.Many(as.iterator.map(summon[ToXmlMod[A]].toMod).toSeq)

  object ToXmlMod extends LowPriorityToXmlMod:
    given xmlMod: ToXmlMod[XmlMod]:
      def toMod(a: XmlMod): XmlMod = a
    given string: ToXmlMod[String]:
      def toMod(s: String): XmlMod = XmlMod.Child(text(s))
    given node: ToXmlMod[Node]:
      def toMod(n: Node): XmlMod = XmlMod.Child(n)
    given option: [A] => ToXmlMod[A] => ToXmlMod[Option[A]]:
      def toMod(a: Option[A]): XmlMod = a match
        case None => XmlMod.Empty
        case Some(v) => summon[ToXmlMod[A]].toMod(v)
    given seq: [A] => ToXmlMod[A] => ToXmlMod[Seq[A]]:
      def toMod(as: Seq[A]): XmlMod = XmlMod.Many(as.map(summon[ToXmlMod[A]].toMod))
    given conversion: [A] => ToXmlMod[A] => Conversion[A, XmlMod]:
      def apply(a: A): XmlMod = summon[ToXmlMod[A]].toMod(a)

  final class Attr(val name: XmlName):
    def :=(value: String): XmlMod = XmlMod.SetAttr(name, value)
    def :=(value: Boolean): XmlMod =
      if value then XmlMod.SetAttr(name, "true") else XmlMod.Empty
    def :=(value: Int): XmlMod = XmlMod.SetAttr(name, value.toString)

  final class MultiAttr(name: XmlName, separator: String = " "):
    def :=(value: String): XmlMod = XmlMod.SetAttr(name, value)
    def +=(value: String): XmlMod = XmlMod.AppendAttr(name, value, separator)

  private def applyMods(element: Element, mods: Seq[XmlMod]): Element =
    val start: (Seq[(XmlName, String)], Nodes) = (element.getAttributes, element.getChildren)
    val (attrs, children) = mods.foldLeft(start)(applyOne)
    this.element(element.getName, attrs, children)

  private def applyOne(
    state: (Seq[(XmlName, String)], Nodes),
    mod: XmlMod
  ): (Seq[(XmlName, String)], Nodes) =
    val (as, cs) = state
    mod match
      case XmlMod.Empty => state
      case XmlMod.Many(inner) => inner.foldLeft(state)(applyOne)
      case XmlMod.SetAttr(name, value) if value.isEmpty =>
        (as.filterNot((n, _) => n.sameAs(name)), cs)
      case XmlMod.SetAttr(name, value) =>
        (as.filterNot((n, _) => n.sameAs(name)).appended(name -> value), cs)
      case XmlMod.AppendAttr(name, value, sep) =>
        as.collectFirst { case (n, v) if n.sameAs(name) => v } match
          case None => (as.appended(name -> value), cs)
          case Some(prev) =>
            (as.filterNot((n, _) => n.sameAs(name)).appended(name -> s"$prev$sep$value"), cs)
      case XmlMod.Child(node) => (as, cs.appended(node))

  private def tagged(name: String, mods: Seq[XmlMod]): Element =
    applyMods(element(name, Seq.empty, Seq.empty), mods)

  final def element(name: String, mods: Conversion.into[XmlMod]*): Element = tagged(name, mods)

  extension (element: Element)
    def when(condition: Boolean)(mods: Conversion.into[XmlMod]*): Element =
      if condition then applyMods(element, mods) else element
    def whenSome[T](option: Option[T])(f: T => Seq[XmlMod]): Element =
      option.fold(element)(v => applyMods(element, f(v)))
    def inlineJs(code: String): Element =
      applyMods(element, Seq(XmlMod.Child(text(code))))
    def externalJs(url: String): Element =
      applyMods(element, Seq(XmlMod.SetAttr(XmlAttribute.Src.name, url)))

  private def attrName(qName: String): XmlName = XmlName.parse(qName, isAttribute = true)

  val className: MultiAttr = MultiAttr(XmlAttribute.CssClass.name)
  val id: Attr = Attr(XmlAttribute.Id.name)
  val href: Attr = Attr(XmlAttribute.Href.name)
  val src: Attr = Attr(XmlAttribute.Src.name)
  val rel: MultiAttr = MultiAttr(XmlAttribute.Rel.name)
  val titleAttr: Attr = Attr(XmlAttribute.Title.name)
  val contentAttr: Attr = Attr(attrName("content"))
  val langAttr: Attr = Attr(XmlAttribute.Lang.name)
  val `type`: Attr = Attr(XmlAttribute.Type.name)
  val typeAttr: Attr = `type`
  val `for`: Attr = Attr(attrName("for"))
  val forAttr: Attr = `for`
  val target: Attr = Attr(XmlAttribute.Target.name)
  val role: Attr = Attr(XmlAttribute.Role.name)
  val name: Attr = Attr(attrName("name"))
  val charset: Attr = Attr(attrName("charset"))
  val httpEquiv: Attr = Attr(attrName("http-equiv"))
  val itemProp: Attr = Attr(attrName("itemprop"))
  val itemType: Attr = Attr(attrName("itemtype"))
  val itemScope: Attr = Attr(attrName("itemscope"))
  val hidden: Attr = Attr(attrName("hidden"))
  val datetime: Attr = Attr(attrName("datetime"))
  val xmlns: Attr = Attr(XmlAttribute.Xmlns.name)

  def aria(name: String): Attr = Attr(attrName(s"aria-$name"))
  def attr(qName: String): Attr = Attr(attrName(qName))
  def dataAttr(name: String): Attr = Attr(attrName(s"data-$name"))

  def a(mods: Conversion.into[XmlMod]*): Element = tagged("a", mods)
  def article(mods: Conversion.into[XmlMod]*): Element = tagged("article", mods)
  def blockquote(mods: Conversion.into[XmlMod]*): Element = tagged("blockquote", mods)
  def body(mods: Conversion.into[XmlMod]*): Element = tagged("body", mods)
  def br(mods: Conversion.into[XmlMod]*): Element = tagged("br", mods)
  def code(mods: Conversion.into[XmlMod]*): Element = tagged("code", mods)
  def data(mods: Conversion.into[XmlMod]*): Element = tagged("data", mods)
  def dd(mods: Conversion.into[XmlMod]*): Element = tagged("dd", mods)
  def details(mods: Conversion.into[XmlMod]*): Element = tagged("details", mods)
  def div(mods: Conversion.into[XmlMod]*): Element = tagged("div", mods)
  def dl(mods: Conversion.into[XmlMod]*): Element = tagged("dl", mods)
  def dt(mods: Conversion.into[XmlMod]*): Element = tagged("dt", mods)
  def em(mods: Conversion.into[XmlMod]*): Element = tagged("em", mods)
  def figcaption(mods: Conversion.into[XmlMod]*): Element = tagged("figcaption", mods)
  def figure(mods: Conversion.into[XmlMod]*): Element = tagged("figure", mods)
  def footer(mods: Conversion.into[XmlMod]*): Element = tagged("footer", mods)
  def h1(mods: Conversion.into[XmlMod]*): Element = tagged("h1", mods)
  def h2(mods: Conversion.into[XmlMod]*): Element = tagged("h2", mods)
  def h3(mods: Conversion.into[XmlMod]*): Element = tagged("h3", mods)
  def head(mods: Conversion.into[XmlMod]*): Element = tagged("head", mods)
  def header(mods: Conversion.into[XmlMod]*): Element = tagged("header", mods)
  def html(mods: Conversion.into[XmlMod]*): Element = tagged("html", mods)
  def img(mods: Conversion.into[XmlMod]*): Element = tagged("img", mods)
  def input(mods: Conversion.into[XmlMod]*): Element = tagged("input", mods)
  def label(mods: Conversion.into[XmlMod]*): Element = tagged("label", mods)
  def li(mods: Conversion.into[XmlMod]*): Element = tagged("li", mods)
  def link(mods: Conversion.into[XmlMod]*): Element = tagged("link", mods)
  def main(mods: Conversion.into[XmlMod]*): Element = tagged("main", mods)
  def meta(mods: Conversion.into[XmlMod]*): Element = tagged("meta", mods)
  def nav(mods: Conversion.into[XmlMod]*): Element = tagged("nav", mods)
  def ol(mods: Conversion.into[XmlMod]*): Element = tagged("ol", mods)
  def p(mods: Conversion.into[XmlMod]*): Element = tagged("p", mods)
  def pre(mods: Conversion.into[XmlMod]*): Element = tagged("pre", mods)
  def script(mods: Conversion.into[XmlMod]*): Element = tagged("script", mods)
  def span(mods: Conversion.into[XmlMod]*): Element = tagged("span", mods)
  def style(mods: Conversion.into[XmlMod]*): Element = tagged("style", mods)
  def summary(mods: Conversion.into[XmlMod]*): Element = tagged("summary", mods)
  def table(mods: Conversion.into[XmlMod]*): Element = tagged("table", mods)
  def td(mods: Conversion.into[XmlMod]*): Element = tagged("td", mods)
  def th(mods: Conversion.into[XmlMod]*): Element = tagged("th", mods)
  def time(mods: Conversion.into[XmlMod]*): Element = tagged("time", mods)
  def title(mods: Conversion.into[XmlMod]*): Element = tagged("title", mods)
  def tr(mods: Conversion.into[XmlMod]*): Element = tagged("tr", mods)
  def ul(mods: Conversion.into[XmlMod]*): Element = tagged("ul", mods)

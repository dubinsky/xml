package org.podval.xml

/** Construction DSL for [[Xml]]: tag functions, `:=` / `+=`, `ToXmlMod` flattening, `.when`, script helpers.
  * Inspired by ZIO Blocks HTML (`zio-blocks-html` 0.0.51).
  * Mixed into [[Xml]] only.
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

  object ToXmlMod:
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
      XmlMod.SetAttr(name, if value then "true" else "")

  final class MultiAttr(name: XmlName, separator: String = " "):
    def :=(value: String): XmlMod = XmlMod.SetAttr(name, value)
    def +=(value: String): XmlMod = XmlMod.AppendAttr(name, value, separator)

  private def applyMods(element: Element, mods: Seq[XmlMod]): Element =
    val start: (Seq[(XmlName, String)], Nodes) = (element.getAttributes, element.getChildren)
    val (attrs, children) = mods.foldLeft(start)(applyOne)
    // After every mod, so a prefixed name sees `xmlns:prefix` even when that declaration is later.
    val rebound: Seq[(XmlName, String)] = attrs.map: (name, value) =>
      XmlName.parseDeclared(name, attrs, isAttribute = true) -> value
    this.element(XmlName.parseDeclared(element.getName, rebound, isAttribute = false), rebound, children)

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

  private def tagged(elem: XmlElement, mods: Seq[XmlMod]): Element =
    applyMods(element(elem), mods)

  private def tagged(name: String, mods: Seq[XmlMod]): Element =
    applyMods(element(name, Seq.empty, Seq.empty), mods)

  final def element(name: String, mods: Conversion.into[XmlMod]*): Element = tagged(name, mods)

  extension (element: Element)
    def when(condition: Boolean)(mods: Conversion.into[XmlMod]*): Element =
      if condition then applyMods(element, mods) else element
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
  val contentAttr: Attr = Attr(XmlAttribute.Content.name)
  val langAttr: Attr = Attr(XmlAttribute.Lang.name)
  val `type`: Attr = Attr(XmlAttribute.Type.name)
  val typeAttr: Attr = `type`
  val `for`: Attr = Attr(XmlAttribute.For.name)
  val forAttr: Attr = `for`
  val target: Attr = Attr(XmlAttribute.Target.name)
  val role: Attr = Attr(XmlAttribute.Role.name)
  val name: Attr = Attr(XmlAttribute.Name.name)
  val charset: Attr = Attr(XmlAttribute.Charset.name)
  val httpEquiv: Attr = Attr(XmlAttribute.HttpEquiv.name)
  val itemProp: Attr = Attr(XmlAttribute.ItemProp.name)
  val itemType: Attr = Attr(XmlAttribute.ItemType.name)
  val itemScope: Attr = Attr(XmlAttribute.ItemScope.name)
  val hidden: Attr = Attr(XmlAttribute.Hidden.name)
  val datetime: Attr = Attr(XmlAttribute.Datetime.name)
  val xmlns: Attr = Attr(XmlAttribute.Xmlns.name)
  def xmlns(prefix: String): Attr = Attr(XmlAttribute.Xmlns(prefix).name)

  def aria(name: String): Attr = Attr(attrName(s"aria-$name"))
  def attr(qName: String): Attr = Attr(attrName(qName))

  def a(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.A, mods)
  def article(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Article, mods)
  def blockquote(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Blockquote, mods)
  def body(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Body, mods)
  def br(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Br, mods)
  def code(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Code, mods)
  def data(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Data, mods)
  def dd(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Dd, mods)
  def details(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Details, mods)
  def div(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Div, mods)
  def dl(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Dl, mods)
  def dt(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Dt, mods)
  def em(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Em, mods)
  def figcaption(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Figcaption, mods)
  def figure(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Figure, mods)
  def footer(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Footer, mods)
  def h1(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.H1, mods)
  def h2(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.H2, mods)
  def h3(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.H3, mods)
  def head(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Head, mods)
  def header(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Header, mods)
  def html(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Html, mods)
  def img(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Img, mods)
  def input(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Input, mods)
  def label(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Label, mods)
  def li(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Li, mods)
  def link(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Link, mods)
  def main(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Main, mods)
  def meta(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Meta, mods)
  def nav(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Nav, mods)
  def ol(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Ol, mods)
  def p(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.P, mods)
  def pre(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Pre, mods)
  def script(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Script, mods)
  def span(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Span, mods)
  def style(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Style, mods)
  def summary(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Summary, mods)
  def table(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Table, mods)
  def td(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Td, mods)
  def th(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Th, mods)
  def time(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Time, mods)
  def title(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Title, mods)
  def tr(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Tr, mods)
  def ul(mods: Conversion.into[XmlMod]*): Element = tagged(XmlElement.Ul, mods)

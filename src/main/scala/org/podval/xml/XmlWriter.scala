package org.podval.xml

import org.typelevel.paiges.Doc

object XmlWriter:
  private val indent: Int = 2

  val widthDefault: Int = 120

  // Paiges `Doc.text` turns real newlines into indentable `Line`s. NUL is illegal
  // in XML 1.0, so it cannot appear in a well-formed tree.
  private val hiddenNewline: Char = '\u0000'

  private def hideNewlines(text: String): String = text.replace('\n', hiddenNewline)

  /** In-scope namespace bindings. `None` is the default xmlns. `xml` starts bound. */
  private final case class NsScope(bindings: Map[Option[String], String]) derives CanEqual:
    def get(prefix: Option[String]): Option[String] = bindings.get(prefix.filter(_.nonEmpty))

    def bind(prefix: Option[String], uri: String): NsScope =
      val key: Option[String] = prefix.filter(_.nonEmpty)
      if uri.isEmpty then NsScope(bindings - key) else NsScope(bindings.updated(key, uri))

  private object NsScope:
    val root: NsScope = NsScope(Map(Some(XmlNamespace.xml.prefix.get) -> XmlNamespace.xml.uri))

  /** `Some(None)` is a default `xmlns`. `Some(Some(prefix))` is `xmlns:prefix`. */
  private def declaredPrefix(name: XmlName): Option[Option[String]] =
    if !name.isXmlnsDeclaration then None
    else if name.isDefaultXmlns then Some(None)
    else Some(Some(name.localName))

  private def attributesAndScope(
    element: Xml.Element,
    parent: NsScope
  ): (Seq[(String, String)], NsScope) =
    val existing: Seq[(XmlName, String)] = element.getAttributes
    val afterDeclared: NsScope = existing.foldLeft(parent): (scope, pair) =>
      val (name, value) = pair
      declaredPrefix(name).fold(scope)(prefix => scope.bind(prefix, value))
    val synthesized: Seq[(XmlName, String)] =
      (element.getName +: existing.map(_._1)).foldLeft(Seq.empty[(XmlName, String)]): (acc, name) =>
        neededDeclaration(name, afterDeclared, acc)
    val child: NsScope = synthesized.foldLeft(afterDeclared): (scope, pair) =>
      val (name, value) = pair
      declaredPrefix(name).fold(scope)(prefix => scope.bind(prefix, value))
    (XmlName.asPairs(synthesized ++ existing), child)

  private def neededDeclaration(
    name: XmlName,
    scope: NsScope,
    already: Seq[(XmlName, String)]
  ): Seq[(XmlName, String)] =
    if name.isXmlnsDeclaration || name.isXml then already
    else name.uri match
      case Some(uri) =>
        val prefix: Option[String] = name.prefix.filter(_.nonEmpty)
        val pending: Boolean = already.exists: (decl, value) =>
          declaredPrefix(decl).contains(prefix) && value == uri
        if scope.get(prefix).contains(uri) || pending then already
        else already :+ XmlName.xmlnsAttribute(prefix, uri)
      case None => already

  private def closesEmpty(using config: XmlWriterConfig)(name: XmlName): Boolean =
    config.selfCloseEmpty || name.localNameIn(config.selfClose)

  def render(config: XmlWriterConfig, element: Xml.Element, width: Int): String =
    fromElement(
      element,
      canBreakLeft = true,
      canBreakRight = true,
      NsScope.root
    )(using config)
      .render(width)
      .replace(hiddenNewline, '\n')
      .appended('\n')

  def render(config: XmlWriterConfig, document: XmlDocument[Xml.Element], width: Int): String =
    document.prefix + render(config, document.root, width) + document.suffix

  private enum Token:
    case Word(value: String)
    case Space()
    case Tree(node: XmlNode)

  private def fromElement(
    element: Xml.Element,
    canBreakLeft: Boolean,
    canBreakRight: Boolean,
    scope: NsScope
  )(using config: XmlWriterConfig): Doc =
    val name: XmlName = element.getName
    if name.localNameIn(config.rawText) then
      Doc.text(rawTextElement(element, scope).mkString(hiddenNewline.toString))
    else if name.localNameIn(config.preformat) then
      Doc.text(preformatElement(element, scope).mkString(hiddenNewline.toString))
    else
      fromMixedElement(element, canBreakLeft, canBreakRight, scope)

  private def fromMixedElement(
    element: Xml.Element,
    canBreakLeft: Boolean,
    canBreakRight: Boolean,
    scope: NsScope
  )(using config: XmlWriterConfig): Doc =
    val (attributeValues: Seq[(String, String)], childScope: NsScope) = attributesAndScope(element, scope)
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(
        Doc.lineOrSpace,
        attributeValues.map((name, value) => Doc.text(s"$name=${XmlEncode.quote(value)}"))
      )

    val tokens: List[Token] = tokenize(element.getChildren.toList)
    val chunks: List[List[Token]] = chunkify(tokens)
    val noText: Boolean = !tokens.exists(hasCharacters)
    val whitespaceLeft: Boolean = tokens.headOption.exists(isSpace)
    val whitespaceRight: Boolean = tokens.lastOption.exists(isSpace)
    val charactersLeft: Boolean = tokens.headOption.exists(hasCharacters)
    val charactersRight: Boolean = tokens.lastOption.exists(hasCharacters)
    val canBreakLeft1: Boolean = canBreakLeft || whitespaceLeft
    val canBreakRight1: Boolean = canBreakRight || whitespaceRight
    val children: List[Doc] = mapEnds(chunks)(
      one = fromChunk(_, canBreakLeft1, canBreakRight1, childScope),
      first = fromChunk(_, canBreakLeft1, true, childScope),
      middle = fromChunk(_, true, true, childScope),
      last = fromChunk(_, true, canBreakRight1, childScope)
    )

    val name: XmlName = element.getName
    val qName: String = name.qName

    if children.isEmpty then
      Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + (
        if closesEmpty(name)
        then Doc.text("/>")
        else Doc.text(s"></$qName>")
      )
    else
      val start: Doc = Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$qName>")

      val stack: Boolean =
        noText &&
        !name.localNameIn(config.unStack) &&
        ((children.length >= 2) || ((children.length == 1) && name.localNameIn(config.stack)))

      if stack then
        // If this is clearly a bunch of elements - stack 'em with an indent:
        Doc.cat(Seq(
          start,
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(XmlWriter.indent))),
          Doc.hardLine,
          end
        ))
      else if name.localNameIn(config.nest) then
        // If this is forced-nested element - nest it:
        Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, XmlWriter.indent)
      else
        // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
        // character content should stick to the opening and closing tags.
        // unStack (phrasing): a break after the start tag or before the end tag is a visible
        // HTML space, e.g. "(<span>\n  <a>posuk</a>" → "( posuk".
        // unStack: a break here would become a visible HTML space.
        // stick: a verse line keeps `<l>` on the first word and `</l>` on the last.
        val breakAtTags: Boolean =
          !name.localNameIn(config.unStack) && !name.localNameIn(config.stick)
        Doc.cat(Seq(
          start,
          if breakAtTags && canBreakLeft && !charactersLeft then Doc.lineOrEmpty else Doc.empty,
          Doc.intercalate(Doc.lineOrSpace, children),
          if breakAtTags && canBreakRight && !charactersRight then Doc.lineOrEmpty else Doc.empty,
          end
        ))

  @scala.annotation.tailrec
  private def tokenize(
    nodes: List[XmlNode],
    acc: List[Token] = Nil
  ): List[Token] = nodes match
    case Nil => acc.reverse
    case n :: ns => n.asText match
      case None => tokenize(ns, Token.Tree(n) :: acc)
      case Some(_) =>
        val (texts, rest) = nodes.span(_.asText.isDefined)
        val more = words(squashBigWhitespace(texts.flatMap(_.asText).mkString))
        tokenize(rest, more.reverse ::: acc)

  private def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  @scala.annotation.tailrec
  private def words(text: String, acc: List[Token] = Nil): List[Token] =
    if text.isEmpty then acc.reverse else
      val (spaces: String, afterSpaces: String) = text.span(_ == ' ')
      val acc1 = if spaces.isEmpty then acc else Token.Space() :: acc
      val (word: String, afterWord: String) = afterSpaces.span(_ != ' ')
      if word.isEmpty then acc1.reverse
      else words(afterWord, Token.Word(word) :: acc1)

  private def isSpace(token: Token): Boolean = token match
    case Token.Space() => true
    case _ => false

  private def hasCharacters(token: Token): Boolean = token match
    case Token.Word(_) => true
    case Token.Space() => false
    case Token.Tree(node) => node.isCharacters

  private def clings(using config: XmlWriterConfig)(
    prev: Token,
    next: Token
  ): Boolean =
    def elementOf(token: Token): Option[Xml.Element] = token match
      case Token.Tree(node) => node.asElement
      case _ => None
    val nextElement: Option[Xml.Element] = elementOf(next)
    elementOf(prev).isEmpty || nextElement.isEmpty ||
      nextElement.exists: el =>
        val name: XmlName = el.getName
        name.localNameIn(config.cling) || name.localNameIn(config.unStack)

  private def chunkify(using config: XmlWriterConfig)(
    tokens: List[Token]
  ): List[List[Token]] =
    @scala.annotation.tailrec
    def loop(
      remaining: List[Token],
      acc: List[List[Token]]
    ): List[List[Token]] = remaining.dropWhile(isSpace) match
      case Nil => acc.reverse
      case head :: tail =>
        @scala.annotation.tailrec
        def take(
          prev: Token,
          rest: List[Token],
          acc: List[Token]
        ): (List[Token], List[Token]) = rest match
          case Nil => (acc.reverse, Nil)
          case Token.Space() :: ns => (acc.reverse, ns)
          case n :: ns if clings(prev, n) => take(n, ns, n :: acc)
          case _ => (acc.reverse, rest)
        val (chunk, after) = take(head, tail, head :: Nil)
        loop(after, chunk :: acc)
    loop(tokens, Nil)

  private def mapEnds[A, B](xs: List[A])(
    one: A => B,
    first: A => B,
    middle: A => B,
    last: A => B
  ): List[B] = xs match
    case Nil => Nil
    case x :: Nil => List(one(x))
    case head :: tail =>
      first(head) :: tail.dropRight(1).map(middle) ::: last(tail.last) :: Nil

  private def fromChunk(using config: XmlWriterConfig)(
    tokens: List[Token],
    canBreakLeft: Boolean,
    canBreakRight: Boolean,
    scope: NsScope
  ): Doc = Doc.cat(mapEnds(tokens)(
    one = token => fromToken(token, canBreakLeft, canBreakRight, scope),
    first = token => fromToken(token, canBreakLeft, canBreakRight = false, scope),
    middle = token => fromToken(token, canBreakLeft = false, canBreakRight = false, scope),
    last = token => fromToken(token, canBreakLeft = false, canBreakRight, scope)
  ))

  private def fromToken(using config: XmlWriterConfig)(
    token: Token,
    canBreakLeft: Boolean,
    canBreakRight: Boolean,
    scope: NsScope
  ): Doc = token match
    case Token.Word(value) => Doc.text(XmlEncode.encodeXmlSpecials(value))
    case Token.Space() => Doc.space
    case Token.Tree(node) => fromNode(node, canBreakLeft, canBreakRight, scope)

  private def fromNode(using config: XmlWriterConfig)(
    node: XmlNode,
    canBreakLeft: Boolean,
    canBreakRight: Boolean,
    scope: NsScope
  ): Doc = node.fold(
    element = (element: Xml.Element) =>
      val result: Doc = fromElement(element, canBreakLeft, canBreakRight, scope)
      // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
      if canBreakRight && element.getName.localNameIn(config.break) then result + Doc.hardLine else result
    ,
    text = value => Doc.text(XmlEncode.encodeXmlSpecials(value)),
    cdata = value => Doc.text(cdataMarkup(value)),
    comment = value => Doc.text(commentMarkup(value)),
    processingInstruction = (target, data) => Doc.text(processingInstructionMarkup(target, data)),
    unknown = Doc.text(XmlEncode.encodeXmlSpecials(node.getText))
  )

  private enum LiteralPart:
    case Run(text: String)
    case Other(node: XmlNode)

  private def coalesce(nodes: Seq[XmlNode], asRun: XmlNode => Option[String]): List[LiteralPart] =
    nodes.foldLeft(List.empty[LiteralPart]): (acc, node) =>
      asRun(node) match
        case Some(text) => acc match
          case LiteralPart.Run(prev) :: rest => LiteralPart.Run(prev + text) :: rest
          case _ => LiteralPart.Run(text) :: acc
        case None => LiteralPart.Other(node) :: acc
    .reverse

  private def splitLines(text: String): Seq[String] =
    text.split("\n", -1).toSeq

  private def attributeText(pairs: Seq[(String, String)]): String =
    if pairs.isEmpty then ""
    else pairs.map((name, value) => s"$name=${XmlEncode.quote(value)}").mkString(" ", " ", "")

  private def wrapLiteral(
    element: Xml.Element,
    attributes: String,
    children: Seq[String]
  )(using config: XmlWriterConfig): Seq[String] =
    val qName: String = element.getName.qName
    if children.isEmpty then
      if closesEmpty(element.getName)
      then Seq(s"<$qName$attributes/>")
      else Seq(s"<$qName$attributes></$qName>")
    else if children.length == 1 then Seq(s"<$qName$attributes>${children.head}</$qName>")
    else Seq(s"<$qName$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$qName>")

  private def rawTextElement(
    element: Xml.Element,
    scope: NsScope
  )(using config: XmlWriterConfig): Seq[String] =
    val (pairs: Seq[(String, String)], childScope: NsScope) = attributesAndScope(element, scope)
    val attributes: String = attributeText(pairs)
    val inner: String = rawInner(element.getChildren, childScope)
    if inner.isEmpty then wrapLiteral(element, attributes, Seq.empty)
    else wrapLiteral(element, attributes, splitLines(XmlEncode.protectHtmlRawText(inner)))

  private def rawInner(using config: XmlWriterConfig)(
    nodes: Seq[XmlNode],
    scope: NsScope
  ): String =
    coalesce(nodes, node => node.asText.orElse(node.asCData)).map:
      case LiteralPart.Run(text) => text
      case LiteralPart.Other(node) => node.fold(
        element = el => rawTextElement(el, scope).mkString("\n"),
        text = identity,
        cdata = identity,
        comment = commentMarkup,
        processingInstruction = (target, data) => processingInstructionMarkup(target, data),
        unknown = node.getText
      )
    .mkString

  private def preformatElement(
    element: Xml.Element,
    scope: NsScope
  )(using config: XmlWriterConfig): Seq[String] =
    val (pairs: Seq[(String, String)], childScope: NsScope) = attributesAndScope(element, scope)
    wrapLiteral(element, attributeText(pairs), preformatChildren(element.getChildren, childScope))

  private def preformatChildren(using config: XmlWriterConfig)(
    nodes: Seq[XmlNode],
    scope: NsScope
  ): Seq[String] =
    coalesce(nodes, _.asText).flatMap:
      case LiteralPart.Run(text) => splitLines(XmlEncode.encodeXmlSpecials(text))
      case LiteralPart.Other(node) => node.fold(
        element = el =>
          if el.getName.localNameIn(config.rawText) then rawTextElement(el, scope)
          else preformatElement(el, scope),
        text = t => splitLines(XmlEncode.encodeXmlSpecials(t)),
        cdata = value => Seq(cdataMarkup(value)),
        comment = value => Seq(commentMarkup(value)),
        processingInstruction = (target, data) => Seq(processingInstructionMarkup(target, data)),
        unknown = splitLines(XmlEncode.encodeXmlSpecials(node.getText))
      )

  private def commentMarkup(value: String): String =
    hideNewlines(XmlNode.Comment(value).markup)

  private def processingInstructionMarkup(target: String, data: String): String =
    hideNewlines(XmlNode.ProcessingInstruction(target, data).markup)

  /** `]]>` is illegal inside one CDATA section; split so the bytes round-trip. */
  private def cdataMarkup(value: String): String =
    def parts(rest: String): List[String] = rest.indexOf("]]>") match
      case -1 => rest :: Nil
      case i =>
        val (left, right) = rest.splitAt(i + 2)
        left :: parts(right)
    hideNewlines(parts(value)
      .map(part => s"<![CDATA[$part]]>")
      .mkString
    )

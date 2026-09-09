package org.podval.xml

import org.typelevel.paiges.Doc

object XmlWriter:
  private val indent: Int = 2

  val widthDefault: Int = 120

  // Paiges `Doc.text` turns real newlines into indentable `Line`s. NUL is illegal
  // in XML 1.0, so it cannot appear in a well-formed tree.
  private val hiddenNewline: Char = '\u0000'

  private def hideNewlines(text: String): String = text.replace('\n', hiddenNewline)

  private def writeAttributes[Element: XmlAst](element: Element): Seq[(String, String)] =
    val existing: Seq[(XmlName, String)] = element.getAttributes
    XmlName.asPairs(
      XmlName.xmlnsDeclarations(element.getName, existing) ++ existing
    )

  def render[Element: XmlAst](config: XmlWriterConfig, element: Element, width: Int): String =
    fromElement(
      element,
      canBreakLeft = true,
      canBreakRight = true
    )(using config)
      .render(width)
      .replace(hiddenNewline, '\n')
      .appended('\n')

  def render[Element: XmlAst](config: XmlWriterConfig, document: XmlDocument[Element], width: Int): String =
    document.prefix + render(config, document.root, width) + document.suffix

  private enum Token[N]:
    case Word(value: String)
    case Space()
    case Tree(node: N)

  private def fromElement[Element](
    element: Element,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  )(using config: XmlWriterConfig)(using ast: XmlAst[Element]): Doc =
    val attributeValues: Seq[(String, String)] = writeAttributes(element)
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(
        Doc.lineOrSpace,
        attributeValues.map((name, value) => Doc.text(s"$name=${XmlEncode.quote(value)}"))
      )

    val tokens: List[Token[ast.Node]] = tokenize(element.getChildren.toList)
    val chunks: List[List[Token[ast.Node]]] = chunkify(tokens)
    val noText: Boolean = !tokens.exists(hasCharacters)
    val whitespaceLeft: Boolean = tokens.headOption.exists(isSpace)
    val whitespaceRight: Boolean = tokens.lastOption.exists(isSpace)
    val charactersLeft: Boolean = tokens.headOption.exists(hasCharacters)
    val charactersRight: Boolean = tokens.lastOption.exists(hasCharacters)
    val canBreakLeft1: Boolean = canBreakLeft || whitespaceLeft
    val canBreakRight1: Boolean = canBreakRight || whitespaceRight
    val children: List[Doc] = mapEnds(chunks)(
      one = fromChunk(_, canBreakLeft1, canBreakRight1),
      first = fromChunk(_, canBreakLeft1, true),
      middle = fromChunk(_, true, true),
      last = fromChunk(_, true, canBreakRight1)
    )

    val name: XmlName = element.getName
    val qName: String = name.qName

    if children.isEmpty then
      Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + (
        if name.localNameIn(config.selfClose)
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
        val breakAtTags: Boolean = !name.localNameIn(config.unStack)
        Doc.cat(Seq(
          start,
          if breakAtTags && canBreakLeft && !charactersLeft then Doc.lineOrEmpty else Doc.empty,
          Doc.intercalate(Doc.lineOrSpace, children),
          if breakAtTags && canBreakRight && !charactersRight then Doc.lineOrEmpty else Doc.empty,
          end
        ))

  @scala.annotation.tailrec
  private def tokenize(using ast: XmlAst[?])(
    nodes: List[ast.Node],
    acc: List[Token[ast.Node]] = Nil
  ): List[Token[ast.Node]] = nodes match
    case Nil => acc.reverse
    case n :: ns => n.asText match
      case None => tokenize(ns, Token.Tree(n) :: acc)
      case Some(_) =>
        val (texts, rest) = nodes.span(_.asText.isDefined)
        val more = words[ast.Node](squashBigWhitespace(texts.flatMap(_.asText).mkString))
        tokenize(rest, more.reverse ::: acc)

  private def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  @scala.annotation.tailrec
  private def words[N](text: String, acc: List[Token[N]] = Nil): List[Token[N]] =
    if text.isEmpty then acc.reverse else
      val (spaces, afterSpaces) = text.span(_ == ' ')
      val acc1 = if spaces.isEmpty then acc else Token.Space() :: acc
      val (word, afterWord) = afterSpaces.span(_ != ' ')
      if word.isEmpty then acc1.reverse
      else words(afterWord, Token.Word(word) :: acc1)

  private def isSpace[N](token: Token[N]): Boolean = token match
    case Token.Space() => true
    case _ => false

  private def hasCharacters(using ast: XmlAst[?])(token: Token[ast.Node]): Boolean = token match
    case Token.Word(_) => true
    case Token.Space() => false
    case Token.Tree(node) => node.isCharacters

  private def clings(using ast: XmlAst[?], config: XmlWriterConfig)(
    prev: Token[ast.Node],
    next: Token[ast.Node]
  ): Boolean =
    def elementOf(token: Token[ast.Node]): Option[ast.Element] = token match
      case Token.Tree(node) => node.asElement
      case _ => None
    val nextElement: Option[ast.Element] = elementOf(next)
    elementOf(prev).isEmpty || nextElement.isEmpty ||
      nextElement.exists: el =>
        val name: XmlName = el.getName
        name.localNameIn(config.cling) || name.localNameIn(config.unStack)

  private def chunkify(using ast: XmlAst[?], config: XmlWriterConfig)(
    tokens: List[Token[ast.Node]]
  ): List[List[Token[ast.Node]]] =
    @scala.annotation.tailrec
    def loop(
      remaining: List[Token[ast.Node]],
      acc: List[List[Token[ast.Node]]]
    ): List[List[Token[ast.Node]]] = remaining.dropWhile(isSpace) match
      case Nil => acc.reverse
      case head :: tail =>
        @scala.annotation.tailrec
        def take(
          prev: Token[ast.Node],
          rest: List[Token[ast.Node]],
          acc: List[Token[ast.Node]]
        ): (List[Token[ast.Node]], List[Token[ast.Node]]) = rest match
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

  private def fromChunk(using config: XmlWriterConfig, ast: XmlAst[?])(
    tokens: List[Token[ast.Node]],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc = Doc.cat(mapEnds(tokens)(
    one = token => fromToken(token, canBreakLeft, canBreakRight),
    first = token => fromToken(token, canBreakLeft, canBreakRight = false),
    middle = token => fromToken(token, canBreakLeft = false, canBreakRight = false),
    last = token => fromToken(token, canBreakLeft = false, canBreakRight)
  ))

  private def fromToken(using config: XmlWriterConfig, ast: XmlAst[?])(
    token: Token[ast.Node],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc = token match
    case Token.Word(value) => Doc.text(XmlEncode.encodeXmlSpecials(value))
    case Token.Space() => Doc.space
    case Token.Tree(node) => fromNode(node, canBreakLeft, canBreakRight)

  private def fromNode(using config: XmlWriterConfig, ast: XmlAst[?])(
    node: ast.Node,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc = node.fold(
    element = (element: ast.Element) =>
      val name: XmlName = element.getName
      if name.localNameIn(config.preformat)
      then
        Doc.text(preformatElement(element).mkString(hiddenNewline.toString))
      else
        val result: Doc = fromElement(element, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
        if canBreakRight && name.localNameIn(config.break) then result + Doc.hardLine else result
    ,
    text = value => Doc.text(XmlEncode.encodeXmlSpecials(value)),
    cdata = value => Doc.text(cdataMarkup(value)),
    comment = value => Doc.text(commentMarkup(value)),
    processingInstruction = (target, data) => Doc.text(processingInstructionMarkup(target, data)),
    unknown = Doc.text(XmlEncode.encodeXmlSpecials(node.getText))
  )

  private def preformatElement[Element: XmlAst](element: Element)(using config: XmlWriterConfig): Seq[String] =
    val attributes: String =
      val pairs: Seq[(String, String)] = writeAttributes(element)
      if pairs.isEmpty then ""
      else pairs.map((name, value) => s"$name=${XmlEncode.quote(value)}").mkString(" ", " ", "")

    val children: Seq[String] = element.getChildren.flatMap(preformat)
    val qName: String = element.getName.qName
    if children.isEmpty then
      if element.getName.localNameIn(config.selfClose)
      then Seq(s"<$qName$attributes/>")
      else Seq(s"<$qName$attributes></$qName>")
    else if children.length == 1 then Seq(s"<$qName$attributes>${children.head}</$qName>")
    else Seq(s"<$qName$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$qName>")

  private def preformat(using ast: XmlAst[?], config: XmlWriterConfig)(node: ast.Node): Seq[String] = node.fold(
    element = preformatElement,
    text = preformat,
    cdata = value => Seq(cdataMarkup(value)),
    comment = value => Seq(commentMarkup(value)),
    processingInstruction = (target, data) => Seq(processingInstructionMarkup(target, data)),
    unknown = preformat(node.getText)
  )

  private def commentMarkup(value: String): String =
    hideNewlines(XmlMisc.Comment(value).markup)

  private def processingInstructionMarkup(target: String, data: String): String =
    hideNewlines(XmlMisc.ProcessingInstruction(target, data).markup)

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

  private def preformat(string: String): Seq[String] =
    XmlEncode.encodeXmlSpecials(string).split("\n").toSeq

package org.podval.xml

import org.typelevel.paiges.Doc

object XmlWriter:
  private val indent: Int = 2

  private def writeAttributes[Element: XmlAst](element: Element): Seq[(String, String)] =
    val existing: Seq[(XmlExpandedName, String)] = element.getExpandedAttributes
    XmlExpandedName.asPairs(
      XmlExpandedName.xmlnsDeclarations(element.getExpandedName, existing) ++ existing
    )

  val widthDefault: Int = 120

  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  private val hiddenNewline: String = "\\n"

  private def hideNewlines(text: String) = text.replace("\n", XmlWriter.hiddenNewline)

  def render[Element: XmlAst](config: XmlWriterConfig, element: Element, width: Int): String =
    fromElement(
      element,
      canBreakLeft = true,
      canBreakRight = true
    )(using config)
      .render(width)
      .replace(XmlWriter.hiddenNewline, "\n")
      .appended('\n')

  def render[Element: XmlAst](config: XmlWriterConfig, document: XmlDocument[Element], width: Int): String =
    document.prefix + render(config, document.root, width) + document.suffix

  private def fromElement[Element](
    element: Element,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  )(using config: XmlWriterConfig)(using ast: XmlAst[Element]): Doc =
    val attributeValues: Seq[(String, String)] = writeAttributes(element)
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map((name, value) =>
        Doc.text(s"$name=") + Doc.lineOrEmpty + Doc.text(XmlEncode.quote(value))
      ))

    val nodes: ast.Nodes = atomize(List.empty, element.getChildren.toList)
    val chunks: Seq[Seq[ast.Node]] = chunkify(Seq.empty, List.empty, nodes.toList, flush = false)
    val noText: Boolean = chunks.forall(_.forall(_.asAtom.isEmpty))
    val whitespaceLeft: Boolean = nodes.headOption.exists(_.isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(_.isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(_.isCharacters)
    val charactersRight: Boolean = nodes.lastOption.exists(_.isCharacters)
    
    val children: Seq[Doc] = if chunks.isEmpty then Seq.empty else
      val canBreakLeft1 = canBreakLeft || whitespaceLeft
      val canBreakRight1 = canBreakRight || whitespaceRight

      if chunks.length == 1 then Seq(
        fromChunk(chunks.head, canBreakLeft1, canBreakRight1)
      ) else
        fromChunk(chunks.head, canBreakLeft = canBreakLeft1, canBreakRight = true) +:
        chunks.tail.init.map(chunk => fromChunk(chunk, canBreakLeft = true, canBreakRight = true)) :+
        fromChunk(chunks.last, canBreakLeft = true, canBreakRight = canBreakRight1)

    val qName: String = element.qName
    val local: String = element.localName

    if children.isEmpty then
      Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + (
        if config.selfClose.contains(local)
        then Doc.text("/>")
        else Doc.text(s"></$qName>")
      )
    else
      val start: Doc = Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$qName>")

      val stack: Boolean =
        noText &&
        !config.unStack.contains(local) &&
        ((children.length >= 2) || ((children.length == 1) && config.stack.contains(local)))

      if stack then
        // If this is clearly a bunch of elements - stack 'em with an indent:
        Doc.cat(Seq(
          start,
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(XmlWriter.indent))),
          Doc.hardLine,
          end
        ))
      else if config.nest.contains(local) then
        // If this is forced-nested element - nest it:
        Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, XmlWriter.indent)
      else
        // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
        // character content should stick to the opening and closing tags.
        // unStack (phrasing): a break after the start tag or before the end tag is a visible
        // HTML space, e.g. "(<span>\n  <a>posuk</a>" → "( posuk".
        val breakAtTags: Boolean = !config.unStack.contains(local)
        Doc.cat(Seq(
          start,
          if breakAtTags && canBreakLeft && !charactersLeft then Doc.lineOrEmpty else Doc.empty,
          Doc.intercalate(Doc.lineOrSpace, children),
          if breakAtTags && canBreakRight && !charactersRight then Doc.lineOrEmpty else Doc.empty,
          end
        ))
  
  @scala.annotation.tailrec
  private def atomize(
    using ast: XmlAst[?]
  )(
    result: ast.Nodes,
    nodes: ast.Nodes
  ): ast.Nodes = if nodes.isEmpty then result else
    val (texts: ast.Nodes, tail: ast.Nodes) = nodes.span(_.asText.isDefined)

    val resultNew: ast.Nodes =
      if texts.isEmpty
      then result
      else result ++ processText(Seq.empty, squashBigWhitespace(texts.map(_.asText.get).mkString("")))

    tail match 
      case Nil => resultNew
      case n :: ns => atomize(resultNew :+ n, ns)

  private def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  @scala.annotation.tailrec
  private def processText(
    using ast: XmlAst[?]
  )(
    result: ast.Nodes,
    text: String
  ): ast.Nodes = if text.isEmpty then result else
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val resultNew: ast.Nodes = if spaces.isEmpty then result else result :+ ast.text(" ")
    val (word: String, tail2: String) = tail.span(_ != ' ')
    if word.isEmpty
    then resultNew
    else processText(resultNew :+ ast.text(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(using dialect: XmlWriterConfig, ast: XmlAst[?])(
    result: Seq[ast.Nodes],
    current: List[ast.Node],
    nodes: List[ast.Node],
    flush: Boolean
  ): Seq[Seq[ast.Node]] =
    if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else
      nodes match
        case Nil =>
          if current.isEmpty then result
          else chunkify(result, current, Nil, flush = true)
        case node :: tail =>
          if node.isWhitespace then
            chunkify(result, current, tail, flush = current.nonEmpty)
          else current match
            case Nil =>
              chunkify(result, node :: current, tail, flush = false)
            case c :: _ if c.isWhitespace =>
              chunkify(result, current, nodes, flush = true)
            case c :: _ =>
              val cling: Boolean =
                c.asElement.isEmpty ||
                c.asElement.nonEmpty && node.asElement.isEmpty && !node.isWhitespace ||
                node.asElement.isDefined && dialect.cling.contains(node.asElement.get.localName)
              if cling
              then chunkify(result, node :: current, tail, flush = false)
              else chunkify(result, current, nodes, flush = true)

  private def fromChunk(using dialect: XmlWriterConfig, ast: XmlAst[?])(
    nodes: ast.Nodes,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    require(nodes.nonEmpty)
    if nodes.length == 1 then
      fromNode(nodes.head, canBreakLeft, canBreakRight)
    else Doc.cat(
      fromNode(nodes.head, canBreakLeft, canBreakRight = false) +:
      nodes.tail.init.map(node => fromNode(node, canBreakLeft = false, canBreakRight = false)) :+
      fromNode(nodes.last, canBreakLeft = false, canBreakRight)
    )
  
  private def fromNode(using dialect: XmlWriterConfig, ast: XmlAst[?])(
    node: ast.Node,
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    node.fold(
      element = (element: ast.Element) =>
        val local: String = element.localName
        if dialect.preformat.contains(local) then
          Doc.text(preformatElement(element).mkString(XmlWriter.hiddenNewline))
        else
          val result: Doc = fromElement(element, canBreakLeft, canBreakRight)
          // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
          if canBreakRight && dialect.break.contains(local) then result + Doc.hardLine else result
      ,
      text = value => Doc.text(XmlEncode.encodeXmlSpecials(value)),
      cdata = value => Doc.text(cdataMarkup(value)),
      comment = value => Doc.text(commentMarkup(value)),
      processingInstruction = (target, data) => Doc.text(processingInstructionMarkup(target, data)),
      unknown = Doc.paragraph(node.getText)
    )

  private def preformatElement[Element: XmlAst](element: Element): Seq[String] =
    val attributeValues: Seq[(String, String)] = writeAttributes(element)
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map((name, value) => s"$name=${XmlEncode.quote(value)}")
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      element.getChildren.flatMap(preformat)

    val qName: String = element.qName
    if children.isEmpty then Seq(s"<$qName$attributes/>")
    else if children.length == 1 then Seq(s"<$qName$attributes>${children.head}</$qName>")
    else Seq(s"<$qName$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$qName>")

  private def preformat(using ast: XmlAst[?])(node: ast.Node): Seq[String] =
    node.fold(
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
    def parts(rest: String): List[String] =
      rest.indexOf("]]>") match
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

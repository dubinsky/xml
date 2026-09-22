package org.podval.xml

import scala.collection.mutable

final class XmlBuilder[E](using ast: XmlAst[E]):
  private final class Frame(
    val name: XmlName,
    val attributes: Seq[(XmlName, String)],
    val children: mutable.ArrayBuffer[ast.Node]
  )

  private val frames: mutable.Stack[Frame] = mutable.Stack.empty

  private var root: Option[E] = None

  private val prologBuf: mutable.ArrayBuffer[XmlMisc] = mutable.ArrayBuffer.empty

  private val epilogueBuf: mutable.ArrayBuffer[XmlMisc] = mutable.ArrayBuffer.empty

  private var doctypeValue: Option[XmlDoctype] = None

  private def done: Boolean = root.nonEmpty

  def result: E =
    require(done, "XmlBuilder has no document element")
    root.get

  def document: XmlDocument[E] = XmlDocument(
    declaration = None,
    doctype = doctypeValue,
    prolog = prologBuf.toSeq,
    root = result,
    epilogue = epilogueBuf.toSeq
  )

  def startElement(name: XmlName, attributes: Seq[(XmlName, String)]): Unit =
    frames.push(Frame(name, attributes, mutable.ArrayBuffer.empty))

  def startElement(element: E): Unit =
    startElement(element.getName, element.getAttributes)
    element.getChildren.foreach(addChild)

  def endElement(): Unit =
    val frame: Frame = frames.pop()
    val element: E = ast.element(frame.name, frame.attributes, frame.children.toSeq)
    if frames.nonEmpty
    then addChild(element)
    else root = Some(element)

  private def addChild(child: ast.Node): Unit =
    if frames.nonEmpty then appendChild(frames.top.children, child)

  def doctype(name: String, publicId: Option[String], systemId: Option[String]): Unit =
    if root.isEmpty then
      doctypeValue = Some(XmlDoctype(name, publicId, systemId))

  def processingInstruction(target: String, data: String): Unit =
    if frames.nonEmpty then ast.processingInstruction(target, data).foreach(addChild)
    else addMisc(XmlNode.ProcessingInstruction(target, data))

  def comment(text: String): Unit =
    if frames.nonEmpty then ast.comment(text).foreach(addChild)
    else addMisc(XmlNode.Comment(text))

  private def addMisc(misc: XmlMisc): Unit =
    if root.isEmpty then prologBuf += misc else epilogueBuf += misc

  /** Parser character chunks of one text run. Consecutive Text nodes merge;
    * CDATA stays a separate child (a CDATA section is delimited). */
  def text(value: String): Unit =
    if value.nonEmpty then addChild(ast.text(value))

  def cdata(value: String): Unit =
    if value.nonEmpty then addChild(ast.cdata(value))

  private def appendChild(children: mutable.ArrayBuffer[ast.Node], child: ast.Node): Unit = child.asText match
    case None => children += child
    case Some(more) => children.lastOption.flatMap(_.asText) match
      case None => children += child
      case Some(prev) =>
        children.dropRightInPlace(1)
        children += ast.text(prev + more)

package org.podval.xml

import scala.collection.mutable

final class XmlBuilder[E](using ast: XmlAst[E]):
  private val elements: mutable.Stack[E] = mutable.Stack.empty

  private var root: Option[E] = None

  private val prologBuf: mutable.ArrayBuffer[XmlMisc] = mutable.ArrayBuffer.empty

  private val epilogBuf: mutable.ArrayBuffer[XmlMisc] = mutable.ArrayBuffer.empty

  private var doctypeValue: Option[XmlDoctype] = None

  def done: Boolean = root.nonEmpty

  def result: E = root.get

  def document: XmlDocument[E] = XmlDocument(
    declaration = None,
    doctype = doctypeValue,
    prolog = prologBuf.toSeq,
    root = result,
    epilog = epilogBuf.toSeq
  )

  def startElement(name: XmlExpandedName, attributes: Seq[(XmlExpandedName, String)]): Unit =
    startElement(ast.element(name, attributes, Seq.empty))

  def startElement(element: E): Unit =
    elements.push(element)

  def endElement(): Unit =
    val element: E = elements.pop()
    if elements.nonEmpty
    then addChild(element)
    else root = Some(element)

  private def addChild(child: ast.Node): Unit =
    if elements.nonEmpty then
      val parent: E = elements.pop()
      elements.push(parent.setChildren(appendChild(parent.getChildren, child)))

  def doctype(name: String, publicId: Option[String], systemId: Option[String]): Unit =
    if root.isEmpty then
      doctypeValue = Some(XmlDoctype(name, publicId, systemId))

  def processingInstruction(target: String, data: String): Unit =
    if elements.nonEmpty then ast.processingInstruction(target, data).foreach(addChild)
    else addMisc(XmlMisc.ProcessingInstruction(target, data))

  def comment(text: String): Unit =
    if elements.nonEmpty then ast.comment(text).foreach(addChild)
    else addMisc(XmlMisc.Comment(text))

  private def addMisc(misc: XmlMisc): Unit =
    if root.isEmpty then prologBuf += misc else epilogBuf += misc

  /** Parser character chunks of one text run. Consecutive Text nodes merge;
    * CDATA stays a separate child (a CDATA section is delimited). */
  def text(value: String): Unit =
    if value.nonEmpty then addChild(ast.text(value))

  def cdata(value: String): Unit =
    if value.nonEmpty then addChild(ast.cdata(value))

  private def appendChild(children: ast.Nodes, child: ast.Node): ast.Nodes = child.asText match
    case Some(more) =>
      children.lastOption.flatMap(_.asText) match
        case Some(prev) => children.dropRight(1) :+ ast.text(prev + more)
        case None => children :+ child
    case None => children :+ child

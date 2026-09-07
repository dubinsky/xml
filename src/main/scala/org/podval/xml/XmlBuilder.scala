package org.podval.xml

import scala.collection.mutable

// TODO make XML prologue and epilogue comments/processing instructions round-trippable
final class XmlBuilder[E](using ast: XmlAst[E]):
  private val elements: mutable.Stack[E] = mutable.Stack.empty

  private var root: Option[E] = None

  // TODO ignore events after done
  def done: Boolean = root.nonEmpty

  def result: E = root.get

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

  def processingInstruction(target: String, data: String): Unit =
    ast.processingInstruction(target, data).foreach(addChild)

  def comment(text: String): Unit =
    ast.comment(text).foreach(addChild)

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

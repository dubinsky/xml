package org.podval.xml

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.Xml
import scala.collection.mutable

// TODO abstract XmlBuilder, XmlParser, and catalog loading over XmlAst and make ZIO Blocks XML optional too.
// TODO make XML prologue and epilogue comments/processing instructions
// round-trippable
final class XmlBuilder:
  private val elements: mutable.Stack[Xml.Element] = mutable.Stack.empty

  private var root: Option[Xml.Element] = None

  // TODO ignore events after done
  def done: Boolean = root.nonEmpty

  def result: Xml.Element = root.get

  def startElement(element: Xml.Element): Unit =
    elements.push(element)

  def endElement(): Unit =
    val element: Xml.Element = elements.pop()
    if elements.nonEmpty
    then addChild(element)
    else root = Some(element)

  private def addChild(child: Xml): Unit =
    if elements.nonEmpty then
      val parent: Xml.Element = elements.pop()
      elements.push(parent.copy(children = appendChild(parent.children, child)))

  def processingInstruction(target: String, data: String): Unit =
    addChild(Xml.ProcessingInstruction(target = target, data = data))

  def comment(text: String): Unit =
    addChild(Xml.Comment(text))

  /** Parser character chunks of one text run. Consecutive Text nodes merge;
    * CDATA stays a separate child (a CDATA section is delimited). */
  def text(value: String): Unit =
    if value.nonEmpty then addChild(Xml.Text(value))

  def cdata(value: String): Unit =
    if value.nonEmpty then addChild(Xml.CData(value))

  private def appendChild(children: Chunk[Xml], child: Xml): Chunk[Xml] = child match
    case Xml.Text(more) =>
      children.lastOption match
        case Some(Xml.Text(prev)) => children.dropRight(1) :+ Xml.Text(prev + more)
        case _ => children :+ child
    case _ => children :+ child

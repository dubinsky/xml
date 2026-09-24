package org.podval.xml

import scala.collection.mutable

/** Builds an [[Xml]] document. Consecutive text chunks merge; CDATA stays a separate child. */
final class XmlBuilder:
  private final class Frame(
    val name: XmlName,
    val attributes: Seq[(XmlName, String)],
    val children: mutable.ArrayBuffer[XmlNode]
  )

  private val frames: mutable.Stack[Frame] = mutable.Stack.empty

  private var root: Option[Xml.Element] = None

  private val prologBuf: mutable.ArrayBuffer[XmlMisc] = mutable.ArrayBuffer.empty

  private val epilogueBuf: mutable.ArrayBuffer[XmlMisc] = mutable.ArrayBuffer.empty

  private var doctypeValue: Option[XmlDoctype] = None

  private def done: Boolean = root.nonEmpty

  def result: Xml.Element =
    require(done, "XmlBuilder has no document element")
    root.get

  def document: XmlDocument[Xml.Element] = XmlDocument(
    declaration = None,
    doctype = doctypeValue,
    prolog = prologBuf.toSeq,
    root = result,
    epilogue = epilogueBuf.toSeq
  )

  def startElement(name: XmlName, attributes: Seq[(XmlName, String)]): Unit =
    frames.push(Frame(name, attributes, mutable.ArrayBuffer.empty))

  def startElement(element: Xml.Element): Unit =
    startElement(element.getName, element.getAttributes)
    element.getChildren.foreach(addChild)

  def endElement(): Unit =
    val frame: Frame = frames.pop()
    val element: Xml.Element = Xml.element(frame.name, frame.attributes, frame.children.toSeq)
    if frames.nonEmpty
    then addChild(element)
    else root = Some(element)

  private def addChild(child: XmlNode): Unit =
    if frames.nonEmpty then appendChild(frames.top.children, child)

  def doctype(name: String, publicId: Option[String], systemId: Option[String]): Unit =
    if root.isEmpty then
      doctypeValue = Some(XmlDoctype(name, publicId, systemId))

  def processingInstruction(target: String, data: String): Unit =
    val instruction: XmlNode.ProcessingInstruction = XmlNode.ProcessingInstruction(target, data)
    if frames.nonEmpty then addChild(instruction) else addMisc(instruction)

  def comment(text: String): Unit =
    val node: XmlNode.Comment = XmlNode.Comment(text)
    if frames.nonEmpty then addChild(node) else addMisc(node)

  private def addMisc(misc: XmlMisc): Unit =
    if root.isEmpty then prologBuf += misc else epilogueBuf += misc

  /** Parser character chunks of one text run. Consecutive Text nodes merge;
    * CDATA stays a separate child (a CDATA section is delimited). */
  def text(value: String): Unit =
    if value.nonEmpty then addChild(Xml.text(value))

  def cdata(value: String): Unit =
    if value.nonEmpty then addChild(Xml.cdata(value))

  private def appendChild(children: mutable.ArrayBuffer[XmlNode], child: XmlNode): Unit = child.asText match
    case None => children += child
    case Some(more) => children.lastOption.flatMap(_.asText) match
      case None => children += child
      case Some(prev) =>
        children.dropRightInPlace(1)
        children += Xml.text(prev + more)

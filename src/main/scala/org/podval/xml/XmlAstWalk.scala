package org.podval.xml

/** Tree walk, node-list edits, and hand-codec helpers. */
private[xml] trait XmlAstWalk[ELEMENT]:
  this: XmlAst[ELEMENT] =>

  enum Rewrite:
    /** One element. The walk then rewrites its children. */
    case Keep(element: Element)
    /** These nodes replace the element. The walk then runs on them. */
    case Replace(nodes: Nodes)

  extension (element: Element)
    def transform(
      transformElement: Element => Element,
      stopAtCode: Boolean = true
    ): Element =
      def loop(element: Element): Element =
        if stopAtCode && element.isNamed(XmlElement.Code.localName) then element
        else
          val result: Element = transformElement(element)
          result.setChildren(result.getChildren.map(xml => xml.asElement.fold(xml)(loop)))
      loop(element)

    def rewrite(
      f: (Element, Option[Element]) => Rewrite,
      stopAtCode: Boolean = true
    ): Element =
      def loop(element: Element, parent: Option[Element]): Nodes =
        if stopAtCode && element.isNamed(XmlElement.Code.localName) then Seq(element)
        else f(element, parent) match
          case Rewrite.Keep(result) =>
            Seq(result.setChildren(descend(result.getChildren, Some(result))))
          case Rewrite.Replace(nodes) =>
            descend(nodes, parent)
      def descend(nodes: Nodes, parent: Option[Element]): Nodes =
        nodes.flatMapNodes: node =>
          node.asElement.fold(Seq(node))(el => loop(el, parent))
      loop(element, None) match
        case Seq(only) if only.asElement.nonEmpty => only.asElement.get
        case _ => throw XmlError("rewrite must leave exactly one element")

    def gather[A](
      gatherElement: Element => Option[A],
      stopAtCode: Boolean = true
    ): Seq[A] =
      def loop(element: Element): Seq[A] =
        val fromElement: Option[A] = gatherElement(element)
        val fromChildren: Seq[A] =
          if stopAtCode && element.isNamed(XmlElement.Code.localName) then Seq.empty
          else element.flatMapElements(loop)
        fromElement.toSeq ++ fromChildren
      loop(element)

    def gatherWithContext[A](
      gatherElement: (Element, Option[Element]) => Option[A],
      isContext: Element => Boolean,
      stopAtCode: Boolean = true
    ): Seq[A] =
      def loop(element: Element, context: Option[Element]): Seq[A] =
        val fromElement: Option[A] = gatherElement(element, context)
        val fromChildren: Seq[A] =
          if stopAtCode && element.isNamed(XmlElement.Code.localName) then Seq.empty
          else
            val contextNew: Option[Element] = if isContext(element) then Some(element) else context
            element.flatMapElements(loop(_, contextNew))
        fromElement.toSeq ++ fromChildren
      loop(element, None)

    def gatherWithParent[A](
      gatherElement: (Element, Option[Element]) => Option[A],
      stopAtCode: Boolean = true
    ): Seq[A] =
      element.gatherWithContext(gatherElement, _ => true, stopAtCode)

    def convertText(converter: String => Nodes): Element =
      element.setChildren(element.getChildren.flatMapNodes(xml =>
        xml.asText.fold(Seq(xml))(converter)
      ))

    def copyAttribute(from: String, to: String): Element =
      element.get(from).fold(element)(element.set(to, _))

    def getById(id: String): Option[Element] = element
      .gather(el => Option.when(el.getId.contains(id))(el))
      .headOption

    def isInclude: Boolean =
      element.getName.isInclude &&
        element.get(XmlAttribute.Href).exists(_.trim.nonEmpty)

    def childrenNamed(name: String): Seq[Element] =
      element.getChildren.flatMap(_.asElement).filter(_.isNamed(name))

    def childNamed(name: String): Option[Element] = element.childrenNamed(name).headOption

    def requireName(name: String): Unit =
      if !element.isNamed(name) then throw XmlError(s"Expected '$name', found '${element.getName.qName}'")

    def requireAttr(name: String): String =
      element.get(name).map(_.trim).filter(_.nonEmpty).getOrElse:
        throw XmlError(s"Missing attribute '$name'")

    def requireNoOther(allowed: Set[String]): Unit =
      val extra: Seq[String] =
        element.getChildren.flatMap(_.asElement).map(_.getName.localName).filterNot(allowed.contains)
      if extra.nonEmpty then throw XmlError(s"Unparsed elements: $extra")

    def positiveInt(name: String): Int =
      val raw: String = element.requireAttr(name)
      val n: Int = raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))
      if n <= 0 then throw XmlError(s"Non-positive integer: $n")
      n

  // ZIO Blocks `Chunk.flatMap` / `++` take ClassTag from the first inner chunk, so a leading
  // text node then an element (or the reverse) throws ArrayStoreException. `:+` uses an AnyRef
  // buffer, as the SAX builder does.
  extension (nodes: Nodes)
    def flatMapNodes(f: Node => Nodes): Nodes =
      nodes.foldLeft(Seq.empty[Node]): (acc, node) =>
        f(node).foldLeft(acc)(_ :+ _)

    def convertElements(converter: Element => Option[Nodes]): Nodes =
      nodes.flatMapNodes(child =>
        child.asElement.flatMap(converter).getOrElse(Seq(child))
      )

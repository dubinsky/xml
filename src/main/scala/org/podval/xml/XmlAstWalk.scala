package org.podval.xml

/** Tree walk, node-list edits, and hand-codec helpers. */
private[xml] trait XmlAstWalk[ELEMENT]:
  this: XmlAst[ELEMENT] =>

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

    def elementById(id: String): Element = element
      .gather(el => Option.when(el.getId.contains(id))(el))
      .head

    def isInclude: Boolean =
      element.getName.isInclude &&
        element.get(XmlAttribute.Href).exists(_.trim.nonEmpty)

    def childrenNamed(name: String): Seq[Element] =
      element.getChildren.flatMap(_.asElement).filter(_.isNamed(name))

    def requireName(name: String): Unit =
      if !element.isNamed(name) then throw XmlError(s"Expected '$name', found '${element.getName.qName}'")

    def requireAttr(name: String): String =
      element.get(name).map(_.trim).filter(_.nonEmpty).getOrElse:
        throw XmlError(s"Missing attribute '$name'")

    def intOpt(name: String): Option[Int] =
      element.get(name).map(_.trim).filter(_.nonEmpty).map: raw =>
        raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))

    def requireNoOther(allowed: Set[String]): Unit =
      val extra: Seq[String] =
        element.getChildren.flatMap(_.asElement).map(_.getName.localName).filterNot(allowed.contains)
      if extra.nonEmpty then throw XmlError(s"Unparsed elements: $extra")

    def intAttr(name: String): Int =
      val raw: String = element.requireAttr(name)
      raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))

    def positiveInt(name: String): Int =
      val n: Int = element.intAttr(name)
      if n <= 0 then throw XmlError(s"Non-positive integer: $n")
      n

    def positiveIntOpt(name: String): Option[Int] =
      element.intOpt(name).map: n =>
        if n <= 0 then throw XmlError(s"Non-positive integer: $n")
        n

    def booleanOpt(name: String): Option[Boolean] =
      element.get(name).map(_.trim).filter(_.nonEmpty).map(XmlAst.parseBoolean)

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

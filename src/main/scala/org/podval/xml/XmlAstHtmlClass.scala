package org.podval.xml

/** HTML `class` attribute helpers. */
private[xml] trait XmlAstHtmlClass[ELEMENT]:
  this: XmlAst[ELEMENT] =>

  extension (element: Element)
    def getClasses: Seq[String] = element
      .get(HtmlClass)
      .fold(Seq.empty): element =>
        element
          .split(' ')
          .toIndexedSeq
          .map(_.trim)
          .filterNot(_.isEmpty)

    def setClasses(values: Seq[String]): Element =
      element.set(HtmlClass, values.mkString(" "))

    def has(htmlClass: HtmlClass): Boolean = hasClass(htmlClass.name)

    def hasClass(htmlClass: String): Boolean = element.getClasses.contains(htmlClass)

    def add(htmlClass: Option[HtmlClass]): Element =
      htmlClass.fold(element)(element.add)

    def add(htmlClass: HtmlClass): Element =
      addClass(htmlClass.name)

    def addClass(htmlClass: String): Element =
      val list = element.getClasses
      if list.contains(htmlClass)
      then element
      else element.setClasses(list.appended(htmlClass))

    def getPrefixedClasses(prefix: String): Seq[String] = element
      .getClasses
      .filter(_.startsWith(s"$prefix-"))
      .map(_.drop(prefix.length + 1))

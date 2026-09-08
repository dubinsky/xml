package org.podval.xml

/** HTML `class` attribute helpers. */
private[xml] trait XmlAstHtmlClass[ELEMENT]:
  this: XmlAst[ELEMENT] =>

  extension (element: Element)
    def getClasses: Seq[String] = element
      .get(XmlAttribute.HtmlClass)
      .fold(Seq.empty): element =>
        element
          .split(' ')
          .toIndexedSeq
          .map(_.trim)
          .filterNot(_.isEmpty)

    def setClasses(values: Seq[String]): Element =
      element.set(XmlAttribute.HtmlClass, values.mkString(" "))

    def has(cssClass: CssClass): Boolean = hasClass(cssClass.name)

    def hasClass(cssClass: String): Boolean = element.getClasses.contains(cssClass)

    def add(cssClass: Option[CssClass]): Element =
      cssClass.fold(element)(element.add)

    def add(cssClass: CssClass): Element =
      addClass(cssClass.name)

    def addClass(cssClass: String): Element =
      val list = element.getClasses
      if list.contains(cssClass)
      then element
      else element.setClasses(list.appended(cssClass))

    def getPrefixedClasses(prefix: String): Seq[String] = element
      .getClasses
      .filter(_.startsWith(s"$prefix-"))
      .map(_.drop(prefix.length + 1))

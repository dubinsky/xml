package org.podval.xml

/** Element operations as members, so `Xml.Element` needs no `XmlAst` given.
  *
  * The algorithms stay on [[XmlAst]]. These methods call them on [[Xml]].
  */
private[xml] trait XmlElementApi:
  this: XmlNode.Element =>

  private def ast: Xml.type = Xml

  def rename(name: String): XmlNode.Element = ast.rename(this)(name)

  def renameKeepingClass(name: String): XmlNode.Element = ast.renameKeepingClass(this)(name)

  def isA: Boolean = ast.isA(this)

  def to[TO](using XmlAst[TO]): TO = ast.to(this)

  def getTextOpt: Option[String] = ast.getTextOpt(this)

  def flatMapElements[A](f: XmlNode.Element => Seq[A]): Seq[A] = ast.flatMapElements(this)(f)

  def setAttributes(attributes: Seq[(XmlName, String)]): XmlNode.Element = ast.setAttributes(this)(attributes)

  def get(attribute: XmlAttribute): Option[String] = ast.get(this)(attribute)

  def get(attribute: String): Option[String] = ast.get(this)(attribute)

  def set(attribute: XmlAttribute, value: String): XmlNode.Element = ast.set(this)(attribute, value)

  def set(attribute: String, value: String): XmlNode.Element = ast.set(this)(attribute, value)

  def set(attribute: XmlAttribute, value: Option[String]): XmlNode.Element = ast.set(this)(attribute, value)

  def set(attribute: String, value: Option[String]): XmlNode.Element = ast.set(this)(attribute, value)

  def getId: Option[String] = ast.getId(this)

  def setId(value: String): XmlNode.Element = ast.setId(this)(value)

  def setId(value: Option[String]): XmlNode.Element = ast.setId(this)(value)

  def copyXmlId: XmlNode.Element = ast.copyXmlId(this)

  def getHref: Option[String] = ast.getHref(this)

  def setHref(value: String): XmlNode.Element = ast.setHref(this)(value)

  def transform(
    transformElement: XmlNode.Element => XmlNode.Element,
    stopAtCode: Boolean = true
  ): XmlNode.Element = ast.transform(this)(transformElement, stopAtCode)

  def rewrite(
    f: (XmlNode.Element, Option[XmlNode.Element]) => Xml.Rewrite,
    stopAtCode: Boolean = true
  ): XmlNode.Element = ast.rewrite(this)(f, stopAtCode)

  def gather[A](
    gatherElement: XmlNode.Element => Option[A],
    stopAtCode: Boolean = true
  ): Seq[A] = ast.gather(this)(gatherElement, stopAtCode)

  def gatherWithContext[A](
    gatherElement: (XmlNode.Element, Option[XmlNode.Element]) => Option[A],
    isContext: XmlNode.Element => Boolean,
    stopAtCode: Boolean = true
  ): Seq[A] = ast.gatherWithContext(this)(gatherElement, isContext, stopAtCode)

  def gatherWithParent[A](
    gatherElement: (XmlNode.Element, Option[XmlNode.Element]) => Option[A],
    stopAtCode: Boolean = true
  ): Seq[A] = ast.gatherWithParent(this)(gatherElement, stopAtCode)

  def convertText(converter: String => Seq[XmlNode]): XmlNode.Element = ast.convertText(this)(converter)

  def copyAttribute(from: String, to: String): XmlNode.Element = ast.copyAttribute(this)(from, to)

  def getById(id: String): Option[XmlNode.Element] = ast.getById(this)(id)

  def isInclude: Boolean = ast.isInclude(this)

  def childrenNamed(name: String): Seq[XmlNode.Element] = ast.childrenNamed(this)(name)

  def childNamed(name: String): Option[XmlNode.Element] = ast.childNamed(this)(name)

  def requireName(name: String): Unit = ast.requireName(this)(name)

  def requireAttr(name: String): String = ast.requireAttr(this)(name)

  def requireNoOther(allowed: Set[String]): Unit = ast.requireNoOther(this)(allowed)

  def positiveInt(name: String): Int = ast.positiveInt(this)(name)

  def getClasses: Seq[String] = ast.getClasses(this)

  def setClasses(values: Seq[String]): XmlNode.Element = ast.setClasses(this)(values)

  def has(cssClass: CssClass): Boolean = ast.has(this)(cssClass)

  def hasClass(cssClass: String): Boolean = ast.hasClass(this)(cssClass)

  def add(cssClass: Option[CssClass]): XmlNode.Element = ast.add(this)(cssClass)

  def add(cssClass: CssClass): XmlNode.Element = ast.add(this)(cssClass)

  def addClass(cssClass: String): XmlNode.Element = ast.addClass(this)(cssClass)

  def getPrefixedClasses(prefix: String): Seq[String] = ast.getPrefixedClasses(this)(prefix)

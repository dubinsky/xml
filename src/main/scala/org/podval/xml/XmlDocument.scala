package org.podval.xml

/** An XML file: declaration, doctype, prolog misc, root, epilogue misc.
  *
  * Write order is declaration, doctype, prolog, root, epilogue. Prolog/epilogue
  * whitespace is not kept; `prefix` and `suffix` put one newline between items.
  */
final case class XmlDocument[+E](
  declaration: Option[XmlDeclaration],
  doctype: Option[XmlDoctype],
  prolog: Seq[XmlMisc],
  root: E,
  epilogue: Seq[XmlMisc]
) derives CanEqual:
  def prefix: String = joined(
    declaration.map(_.markup).toSeq ++ doctype.map(_.markup) ++ prolog.map(_.markup)
  )

  def suffix: String = joined(epilogue.map(_.markup))

  private def joined(parts: Seq[String]): String =
    if parts.isEmpty then "" else parts.mkString("\n") + "\n"

object XmlDocument:
  def xml[E](
    root: E,
    prolog: Seq[XmlMisc] = Seq.empty,
    epilog: Seq[XmlMisc] = Seq.empty,
    doctype: Option[XmlDoctype] = None,
    declaration: XmlDeclaration = XmlDeclaration()
  ): XmlDocument[E] =
    XmlDocument(
      declaration = Some(declaration),
      doctype = doctype,
      prolog = prolog,
      root = root,
      epilogue = epilog
    )

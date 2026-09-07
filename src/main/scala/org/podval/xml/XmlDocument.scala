package org.podval.xml

/** An XML file: declaration, doctype, prolog misc, root, epilog misc.
  *
  * Write order is declaration, doctype, prolog, root, epilog. Prolog/epilog
  * whitespace is not kept; the writer puts one newline between items.
  */
final case class XmlDocument[+E](
  declaration: Option[XmlDeclaration],
  doctype: Option[XmlDoctype],
  prolog: Seq[XmlMisc],
  root: E,
  epilog: Seq[XmlMisc]
) derives CanEqual

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
      epilog = epilog
    )

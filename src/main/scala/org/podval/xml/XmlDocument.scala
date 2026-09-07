package org.podval.xml

/** Comment or processing instruction outside the document element (`Misc` minus whitespace). */
enum XmlMisc derives CanEqual:
  case Comment(text: String)
  case ProcessingInstruction(target: String, data: String)

/** XML declaration. Parse always stores the canonical defaults; write emits them as-is. */
final case class XmlDeclaration(
  version: String = "1.0",
  encoding: Option[String] = Some("UTF-8"),
  standalone: Option[Boolean] = None
) derives CanEqual

final case class XmlDoctype(
  name: String,
  publicId: Option[String] = None,
  systemId: Option[String] = None
) derives CanEqual

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

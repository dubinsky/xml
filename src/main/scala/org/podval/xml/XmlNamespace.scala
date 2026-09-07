package org.podval.xml

/* from https://www.w3.org/TR/xml-names/

  The prefix xml is by definition bound to the namespace name http://www.w3.org/XML/1998/namespace.
  It MAY, but need not, be declared, and MUST NOT be bound to any other namespace name.
  Other prefixes MUST NOT be bound to this namespace name, and it MUST NOT be declared as the default namespace.

  The prefix xmlns is used only to declare namespace bindings and is by definition bound to the namespace
  name http://www.w3.org/2000/xmlns/. It MUST NOT be declared. Other prefixes MUST NOT be bound to this namespace name,
  and it MUST NOT be declared as the default namespace. Element names MUST NOT have the prefix xmlns.

  All other prefixes beginning with the three-letter sequence x, m, l, in any case combination, are reserved.
  This means that:
    users SHOULD NOT use them except as defined by later specifications
    processors MUST NOT treat them as fatal errors.

  If there is no default namespace declaration in scope, the namespace name has no value.
  The namespace name for an unprefixed attribute name always has no value.
*/
object XmlNamespace:
  val xml: String = "http://www.w3.org/XML/1998/namespace"

  val xmlns: String = "http://www.w3.org/2000/xmlns/"

  val xhtml: String = "http://www.w3.org/1999/xhtml"

  val xinclude: String = "http://www.w3.org/2001/XInclude"

  val xlink: String = "http://www.w3.org/1999/xlink"

  def wellKnown(prefix: Option[String], local: String, isAttribute: Boolean): Option[String] =
    prefix match
      case Some("xml") => Some(xml)
      case Some("xmlns") => Some(xmlns)
      case None if isAttribute && local == "xmlns" => Some(xmlns)
      case _ => None



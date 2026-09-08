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
final case class XmlNamespace(
  uri: String,
  prefix: Option[String]
) derives CanEqual

object XmlNamespace:
  val xml: XmlNamespace = XmlNamespace("http://www.w3.org/XML/1998/namespace", Some("xml"))

  val xmlns: XmlNamespace = XmlNamespace("http://www.w3.org/2000/xmlns/", Some("xmlns"))

  val xhtml: XmlNamespace = XmlNamespace("http://www.w3.org/1999/xhtml", None)

  val xinclude: XmlNamespace = XmlNamespace("http://www.w3.org/2001/XInclude", Some("xi"))

  val xlink: XmlNamespace = XmlNamespace("http://www.w3.org/1999/xlink", Some("xlink"))

  def wellKnown(prefix: Option[String], local: String, isAttribute: Boolean): Option[XmlNamespace] = prefix match
    case p if p == xml.prefix => Some(xml)
    case p if p == xmlns.prefix => Some(xmlns)
    case None if isAttribute && xmlns.prefix.contains(local) => Some(xmlns)
    case _ => None

  def isInclude(localName: String, namespace: Option[String], prefix: Option[String]): Boolean =
    localName == "include" &&
      (namespace.contains(xinclude.uri) || prefix.contains("xi"))

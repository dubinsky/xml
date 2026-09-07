package org.podval.xml

/** XML declaration. Parse always stores the canonical defaults; write emits them as-is. */
final case class XmlDeclaration(
  version: String = "1.0",
  encoding: Option[String] = Some("UTF-8"),
  standalone: Option[Boolean] = None
) derives CanEqual

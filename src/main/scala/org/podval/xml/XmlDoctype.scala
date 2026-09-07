package org.podval.xml

final case class XmlDoctype(
  name: String,
  publicId: Option[String] = None,
  systemId: Option[String] = None
) derives CanEqual

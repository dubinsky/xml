package org.podval.xml

final case class XmlDoctype(
  name: String,
  publicId: Option[String] = None,
  systemId: Option[String] = None
) derives CanEqual:
  def markup: String = (publicId, systemId) match
    case (Some(publicId), Some(systemId)) =>
      s"""<!DOCTYPE $name PUBLIC "$publicId" "$systemId">"""
    case (None, Some(systemId)) =>
      s"""<!DOCTYPE $name SYSTEM "$systemId">"""
    case (Some(publicId), None) =>
      s"""<!DOCTYPE $name PUBLIC "$publicId">"""
    case (None, None) =>
      s"<!DOCTYPE $name>"

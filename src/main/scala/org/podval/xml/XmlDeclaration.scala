package org.podval.xml

/** XML declaration. Parse always stores the canonical defaults; write emits them as-is. */
final case class XmlDeclaration(
  version: String = "1.0",
  encoding: Option[String] = Some("UTF-8"),
  standalone: Option[Boolean] = None
) derives CanEqual:
  def markup: String =
    val encodingAttr: String = encoding.fold("")(value => s""" encoding="$value"""")
    val standaloneAttr: String = standalone.fold(""): value =>
      s""" standalone="${if value then "yes" else "no"}""""
    s"""<?xml version="$version"$encodingAttr$standaloneAttr?>"""

package org.podval.xml

final class XmlError(message: String, cause: Throwable) extends Exception(message, cause):
  def this(message: String) = this(message, null)

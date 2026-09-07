package org.podval.xml

/** Comment or processing instruction outside the document element (`Misc` minus whitespace). */
sealed trait XmlMisc derives CanEqual:
  def markup: String

object XmlMisc:
  final case class Comment(text: String) extends XmlMisc derives CanEqual:
    def markup: String = s"<!--$text-->"

  final case class ProcessingInstruction(target: String, data: String) extends XmlMisc derives CanEqual:
    def markup: String =
      if data.isEmpty then s"<?$target?>" else s"<?$target $data?>"

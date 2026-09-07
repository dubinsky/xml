package org.podval.xml

/** Comment or processing instruction outside the document element (`Misc` minus whitespace). */
enum XmlMisc derives CanEqual:
  case Comment(text: String)
  case ProcessingInstruction(target: String, data: String)

  def markup: String = this match
    case XmlMisc.Comment(text) => XmlMisc.commentMarkup(text)
    case XmlMisc.ProcessingInstruction(target, data) => XmlMisc.processingInstructionMarkup(target, data)

object XmlMisc:
  def commentMarkup(text: String): String = s"<!--$text-->"

  def processingInstructionMarkup(target: String, data: String): String =
    if data.isEmpty then s"<?$target?>" else s"<?$target $data?>"

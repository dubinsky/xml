package org.podval.xml

/** Comment or processing instruction outside the document element (`Misc` minus whitespace). */
enum XmlMisc derives CanEqual:
  case Comment(text: String)
  case ProcessingInstruction(target: String, data: String)

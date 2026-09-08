package org.podval.xml

import scala.annotation.tailrec

object XmlEncode:
  /** Encode `&` and `<` in text (and, via [[quote]], `"` in attributes).
    *
    * Ampersands that already start an entity (`&nbsp;`, `&lt;`, `&#x21A9;`) are
    * left alone: SAX `skippedEntity` stores undeclared HTML names as those
    * characters (Markdown HTML-as-XML). A decoded `&lt;` in the tree (Markdown
    * source `&amp;lt;` outside `<pre>`) therefore stays `&lt;` and browsers
    * show `<`.
    */
  def encodeXmlSpecials(string: String): String =
    encodeAmpersands(string).replace("<", "&lt;")

  // Note: maybe use single quotes if the value contains double quote?
  def quote(value: String): String =
    "\"" + encodeXmlSpecials(value).replace("\"", "&quot;") + "\""

  private def encodeAmpersands(string: String): String =
    @tailrec
    def loop(rest: String, out: StringBuilder): String =
      val (plain, fromAmp) = rest.span(_ != '&')
      out.append(plain)
      if fromAmp.isEmpty then out.toString
      else
        entity(fromAmp) match
          case Some(ent) => loop(fromAmp.drop(ent.length), out.append(ent))
          case None => loop(fromAmp.drop(1), out.append("&amp;"))
    loop(string, StringBuilder())

  /** Well-formed entity at the start of `fromAmp` (`fromAmp` starts with `&`). */
  private def entity(fromAmp: String): Option[String] =
    val afterAmp: String = fromAmp.drop(1)
    val name: Option[String] =
      afterAmp.headOption match
        case Some('#') =>
          val afterHash: String = afterAmp.drop(1)
          afterHash.headOption match
            case Some(x) if x == 'x' || x == 'X' =>
              val digits: String = afterHash.drop(1).takeWhile(isHex)
              Option.when(digits.nonEmpty)(s"#$x$digits")
            case _ =>
              val digits: String = afterHash.takeWhile(_.isDigit)
              Option.when(digits.nonEmpty)(s"#$digits")
        case Some(c) if isNameStart(c) =>
          Some(afterAmp.takeWhile(isNameChar))
        case _ => None
    name.filter(n => afterAmp.drop(n.length).startsWith(";")).map(n => s"&$n;")

  private def isNameStart(c: Char): Boolean =
    c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

  private def isNameChar(c: Char): Boolean =
    isNameStart(c) || c.isDigit

  private def isHex(c: Char): Boolean =
    c.isDigit || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'
